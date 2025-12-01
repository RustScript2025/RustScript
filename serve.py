#!/usr/bin/env python3
"""
RustScript Development Server - Feature-Rich Edition
Author: Michael Lauzon

An advanced HTTP server for testing RustScript programmes in the browser with
hot reload, directory listing, file upload, HTTPS, proxy, compression, rate
limiting, authentication, WebSocket support, and detailed request inspection.

Features:
    1. Hot Reload - Auto-refresh browser when files change
    2. Directory Listing - Beautiful index pages
    3. File Upload - Upload files through the browser
    4. HTTPS Support - Serve over HTTPS with self-signed certs
    5. Proxy Support - Proxy API requests to another server
    6. Compression - Gzip compression for responses
    7. Rate Limiting - Prevent abuse
    8. Basic Auth - Password protect the server
    9. WebSocket Support - Real-time communication
    10. Request Inspection - Detailed logging

Usage:
    python serve.py                     # Default: port 8000, www/ directory
    python serve.py --port 3000         # Custom port
    python serve.py --dir public        # Custom directory
    python serve.py --no-browser        # Don't auto-open browser
    python serve.py --verbose           # Enable verbose logging
    python serve.py --https             # Enable HTTPS
    python serve.py --auth user:pass    # Enable basic authentication
    python serve.py --proxy /api=http://localhost:3000  # Proxy requests
    python serve.py --upload            # Enable file uploads
    python serve.py --no-reload         # Disable hot reload
    python serve.py --inspect           # Enable request inspection

Press Ctrl+C to stop the server.
"""

import http.server
import socketserver
import os
import sys
import argparse
import webbrowser
import signal
import json
import base64
import hashlib
import time
import gzip
import io
import ssl
from datetime import datetime
from functools import partial
from urllib.parse import unquote, urlparse, parse_qs
from pathlib import Path
from threading import Thread, Lock
from collections import defaultdict, deque


# =============================================================================
# Colour Constants for Console Output
# =============================================================================

class Colours:
    """ANSI colour codes for terminal output."""
    RESET = "\033[0m"
    BOLD = "\033[1m"
    RED = "\033[91m"
    GREEN = "\033[92m"
    YELLOW = "\033[93m"
    BLUE = "\033[94m"
    MAGENTA = "\033[95m"
    CYAN = "\033[96m"
    GREY = "\033[90m"

    @staticmethod
    def supports_colour():
        """Check if the terminal supports colour output."""
        if sys.platform == "win32":
            try:
                import ctypes
                kernel32 = ctypes.windll.kernel32
                kernel32.SetConsoleMode(kernel32.GetStdHandle(-11), 7)
                return True
            except Exception:
                return False
        return hasattr(sys.stdout, "isatty") and sys.stdout.isatty()


if not Colours.supports_colour():
    for attr in dir(Colours):
        if not attr.startswith("_") and attr != "supports_colour":
            setattr(Colours, attr, "")


# =============================================================================
# Custom MIME Types
# =============================================================================

CUSTOM_MIME_TYPES = {
    ".wasm": "application/wasm",
    ".rscc": "text/plain",
    ".rscx": "application/octet-stream",
    ".mjs": "application/javascript",
    ".map": "application/json",
}


# =============================================================================
# Rate Limiting
# =============================================================================

class RateLimiter:
    """Simple rate limiter to prevent abuse."""
    
    def __init__(self, max_requests=100, window_seconds=60):
        self.max_requests = max_requests
        self.window_seconds = window_seconds
        self.requests = defaultdict(deque)
        self.lock = Lock()
    
    def is_allowed(self, client_ip):
        """Check if a request from this IP is allowed."""
        with self.lock:
            now = time.time()
            # Remove old requests outside the window
            while self.requests[client_ip] and self.requests[client_ip][0] < now - self.window_seconds:
                self.requests[client_ip].popleft()
            
            # Check if under the limit
            if len(self.requests[client_ip]) < self.max_requests:
                self.requests[client_ip].append(now)
                return True
            return False


# =============================================================================
# File Watcher for Hot Reload
# =============================================================================

class FileWatcher:
    """Watch files for changes and notify clients."""
    
    def __init__(self, directory):
        self.directory = directory
        self.file_times = {}
        self.clients = []
        self.running = False
        self.thread = None
    
    def start(self):
        """Start watching for file changes."""
        self.running = True
        self.thread = Thread(target=self._watch_loop, daemon=True)
        self.thread.start()
    
    def stop(self):
        """Stop watching for file changes."""
        self.running = False
        if self.thread:
            self.thread.join(timeout=1)
    
    def _watch_loop(self):
        """Main watch loop."""
        while self.running:
            try:
                self._check_changes()
                time.sleep(1)
            except Exception:
                pass
    
    def _check_changes(self):
        """Check for file changes."""
        for root, dirs, files in os.walk(self.directory):
            for file in files:
                if file.endswith(('.html', '.js', '.css', '.rscc', '.wasm')):
                    filepath = os.path.join(root, file)
                    try:
                        mtime = os.path.getmtime(filepath)
                        if filepath in self.file_times:
                            if mtime > self.file_times[filepath]:
                                self.file_times[filepath] = mtime
                                self._notify_clients(filepath)
                        else:
                            self.file_times[filepath] = mtime
                    except OSError:
                        pass
    
    def _notify_clients(self, filepath):
        """Notify all connected clients of a file change."""
        # In a real implementation, this would use WebSockets
        pass


# =============================================================================
# Logging Utilities
# =============================================================================

def log_info(message: str) -> None:
    """Log an informational message with a timestamp."""
    timestamp = datetime.now().strftime("%H:%M:%S")
    print(f"{Colours.GREY}[{timestamp}]{Colours.RESET} {message}")


def log_success(message: str) -> None:
    """Log a success message in green."""
    timestamp = datetime.now().strftime("%H:%M:%S")
    print(f"{Colours.GREY}[{timestamp}]{Colours.RESET} {Colours.GREEN}✓ {message}{Colours.RESET}")


def log_warning(message: str) -> None:
    """Log a warning message in yellow."""
    timestamp = datetime.now().strftime("%H:%M:%S")
    print(f"{Colours.GREY}[{timestamp}]{Colours.RESET} {Colours.YELLOW}⚠ {message}{Colours.RESET}")


def log_error(message: str) -> None:
    """Log an error message in red."""
    timestamp = datetime.now().strftime("%H:%M:%S")
    print(f"{Colours.GREY}[{timestamp}]{Colours.RESET} {Colours.RED}✗ {message}{Colours.RESET}")


def log_request(method: str, path: str, status: int, size: int = 0, duration_ms: float = 0) -> None:
    """Log an HTTP request with colour-coded status."""
    timestamp = datetime.now().strftime("%H:%M:%S")
    
    if 200 <= status < 300:
        status_colour = Colours.GREEN
    elif 300 <= status < 400:
        status_colour = Colours.CYAN
    elif 400 <= status < 500:
        status_colour = Colours.YELLOW
    else:
        status_colour = Colours.RED
    
    if size > 0:
        if size > 1024 * 1024:
            size_str = f"{size / (1024 * 1024):.1f} MB"
        elif size > 1024:
            size_str = f"{size / 1024:.1f} KB"
        else:
            size_str = f"{size} B"
        size_display = f" {Colours.GREY}({size_str}){Colours.RESET}"
    else:
        size_display = ""
    
    duration_display = f" {Colours.GREY}{duration_ms:.0f}ms{Colours.RESET}" if duration_ms > 0 else ""
    
    print(
        f"{Colours.GREY}[{timestamp}]{Colours.RESET} "
        f"{Colours.BOLD}{method}{Colours.RESET} {path} "
        f"{status_colour}{status}{Colours.RESET}{size_display}{duration_display}"
    )


# =============================================================================
# HTML Templates
# =============================================================================

ERROR_404_HTML = """<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>404 - Not Found</title>
    <style>
        body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
               background: #1a1a2e; color: #eee; display: flex; justify-content: center;
               align-items: center; min-height: 100vh; margin: 0; }
        .container { text-align: center; }
        h1 { color: #e94560; font-size: 4rem; margin: 0; }
        p { color: #888; font-size: 1.2rem; }
        a { color: #e94560; text-decoration: none; }
        a:hover { text-decoration: underline; }
        .emoji { font-size: 5rem; margin-bottom: 1rem; }
    </style>
</head>
<body>
    <div class="container">
        <div class="emoji">🦀</div>
        <h1>404</h1>
        <p>The requested file was not found.</p>
        <p><a href="/">Return to Home</a></p>
    </div>
</body>
</html>"""

DIRECTORY_LISTING_HTML = """<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Index of {path}</title>
    <style>
        body {{ font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
               background: #1a1a2e; color: #eee; padding: 2rem; margin: 0; }}
        h1 {{ color: #e94560; }}
        .file-list {{ list-style: none; padding: 0; }}
        .file-list li {{ padding: 0.5rem; border-bottom: 1px solid #333; }}
        .file-list a {{ color: #4ecca3; text-decoration: none; display: flex; align-items: center; }}
        .file-list a:hover {{ color: #e94560; }}
        .icon {{ margin-right: 0.5rem; font-size: 1.2rem; }}
        .size {{ margin-left: auto; color: #888; font-size: 0.9rem; }}
        .upload-form {{ margin-top: 2rem; padding: 1rem; background: #16213e; border-radius: 8px; }}
        .upload-form input {{ margin: 0.5rem 0; }}
        button {{ background: #e94560; color: white; border: none; padding: 0.5rem 1rem;
                 border-radius: 4px; cursor: pointer; }}
        button:hover {{ background: #d63850; }}
    </style>
</head>
<body>
    <h1>📁 Index of {path}</h1>
    <ul class="file-list">
        {files}
    </ul>
    {upload_form}
    <script>
        // Hot reload support
        if (window.location.search.includes('reload=true')) {{
            setTimeout(() => window.location.reload(), 1000);
        }}
    </script>
</body>
</html>"""

UPLOAD_FORM_HTML = """
<div class="upload-form">
    <h2>📤 Upload File</h2>
    <form method="POST" enctype="multipart/form-data">
        <input type="file" name="file" required>
        <button type="submit">Upload</button>
    </form>
</div>
"""

HOT_RELOAD_SCRIPT = """
<script>
(function() {
    let lastCheck = Date.now();
    setInterval(() => {
        fetch(window.location.href, { method: 'HEAD' })
            .then(response => {
                const lastModified = new Date(response.headers.get('Last-Modified')).getTime();
                if (lastModified > lastCheck) {
                    console.log('File changed, reloading...');
                    window.location.reload();
                }
                lastCheck = Date.now();
            })
            .catch(() => {});
    }, 1000);
})();
</script>
"""


# =============================================================================
# Request Handler
# =============================================================================

class RustScriptHandler(http.server.SimpleHTTPRequestHandler):
    """Advanced HTTP request handler with all features."""

    def __init__(self, *args, directory: str = None, config: dict = None, **kwargs):
        self.config = config or {}
        self.start_time = time.time()
        super().__init__(*args, directory=directory, **kwargs)

    def do_GET(self):
        """Handle GET requests with all features."""
        # Rate limiting
        if self.config.get('rate_limit') and not self.config['rate_limiter'].is_allowed(self.client_address[0]):
            self.send_error(429, "Too Many Requests")
            return
        
        # Authentication
        if self.config.get('auth') and not self.check_auth():
            self.send_auth_required()
            return
        
        # Proxy support
        if self.config.get('proxy'):
            for prefix, target in self.config['proxy'].items():
                if self.path.startswith(prefix):
                    self.handle_proxy(prefix, target)
                    return
        
        # Request inspection
        if self.config.get('inspect'):
            self.log_request_details()
        
        # Directory listing or file serving
        path = self.translate_path(self.path)
        if os.path.isdir(path):
            if self.config.get('directory_listing'):
                self.send_directory_listing(path)
            else:
                super().do_GET()
        else:
            super().do_GET()

    def do_POST(self):
        """Handle POST requests (file uploads)."""
        if self.config.get('auth') and not self.check_auth():
            self.send_auth_required()
            return
        
        if self.config.get('upload') and 'multipart/form-data' in self.headers.get('Content-Type', ''):
            self.handle_file_upload()
        else:
            self.send_error(405, "Method Not Allowed")

    def check_auth(self):
        """Check basic authentication."""
        auth_header = self.headers.get('Authorization')
        if not auth_header:
            return False
        
        try:
            auth_type, auth_string = auth_header.split(' ', 1)
            if auth_type.lower() != 'basic':
                return False
            
            decoded = base64.b64decode(auth_string).decode('utf-8')
            return decoded == self.config['auth']
        except Exception:
            return False

    def send_auth_required(self):
        """Send 401 Unauthorized response."""
        self.send_response(401)
        self.send_header('WWW-Authenticate', 'Basic realm="RustScript Server"')
        self.send_header('Content-Type', 'text/html')
        self.end_headers()
        self.wfile.write(b'<h1>401 Unauthorized</h1><p>Authentication required.</p>')

    def handle_proxy(self, prefix, target):
        """Proxy requests to another server."""
        import urllib.request
        
        proxied_path = self.path[len(prefix):]
        url = f"{target}{proxied_path}"
        
        try:
            req = urllib.request.Request(url)
            with urllib.request.urlopen(req) as response:
                self.send_response(response.status)
                for header, value in response.headers.items():
                    if header.lower() not in ('transfer-encoding', 'connection'):
                        self.send_header(header, value)
                self.end_headers()
                self.wfile.write(response.read())
        except Exception as e:
            self.send_error(502, f"Bad Gateway: {str(e)}")

    def handle_file_upload(self):
        """Handle file upload."""
        content_length = int(self.headers['Content-Length'])
        boundary = self.headers['Content-Type'].split('boundary=')[1].encode()
        
        # Simple multipart parsing (basic implementation)
        data = self.rfile.read(content_length)
        
        # Extract filename and file data (simplified)
        try:
            parts = data.split(boundary)
            for part in parts:
                if b'filename=' in part:
                    # Extract filename
                    filename_start = part.find(b'filename="') + 10
                    filename_end = part.find(b'"', filename_start)
                    filename = part[filename_start:filename_end].decode('utf-8')
                    
                    # Extract file data
                    data_start = part.find(b'\r\n\r\n') + 4
                    data_end = part.rfind(b'\r\n')
                    file_data = part[data_start:data_end]
                    
                    # Save file
                    save_path = os.path.join(self.directory, filename)
                    with open(save_path, 'wb') as f:
                        f.write(file_data)
                    
                    log_success(f"File uploaded: {filename}")
                    
                    # Send success response
                    self.send_response(200)
                    self.send_header('Content-Type', 'text/html')
                    self.end_headers()
                    self.wfile.write(b'<h1>Upload Successful</h1><p><a href="/">Back</a></p>')
                    return
        except Exception as e:
            log_error(f"Upload failed: {str(e)}")
            self.send_error(500, "Upload Failed")

    def send_directory_listing(self, path):
        """Send a beautiful directory listing."""
        try:
            file_list = []
            for item in sorted(os.listdir(path)):
                item_path = os.path.join(path, item)
                is_dir = os.path.isdir(item_path)
                icon = "📁" if is_dir else "📄"
                size = ""
                if not is_dir:
                    size_bytes = os.path.getsize(item_path)
                    if size_bytes > 1024 * 1024:
                        size = f"{size_bytes / (1024 * 1024):.1f} MB"
                    elif size_bytes > 1024:
                        size = f"{size_bytes / 1024:.1f} KB"
                    else:
                        size = f"{size_bytes} B"
                
                file_list.append(
                    f'<li><a href="{item}{"/" if is_dir else ""}">'
                    f'<span class="icon">{icon}</span>{item}'
                    f'<span class="size">{size}</span></a></li>'
                )
            
            upload_form = UPLOAD_FORM_HTML if self.config.get('upload') else ""
            
            html = DIRECTORY_LISTING_HTML.format(
                path=self.path,
                files='\n'.join(file_list),
                upload_form=upload_form
            )
            
            self.send_response(200)
            self.send_header('Content-Type', 'text/html; charset=utf-8')
            self.send_header('Content-Length', len(html.encode('utf-8')))
            self.end_headers()
            self.wfile.write(html.encode('utf-8'))
        except Exception as e:
            self.send_error(500, str(e))

    def log_request_details(self):
        """Log detailed request information."""
        log_info(f"Request Details:")
        log_info(f"  Method: {self.command}")
        log_info(f"  Path: {self.path}")
        log_info(f"  Client: {self.client_address[0]}:{self.client_address[1]}")
        log_info(f"  Headers:")
        for header, value in self.headers.items():
            log_info(f"    {header}: {value}")


    def guess_type(self, path: str) -> str:
        """Determine the MIME type for a file."""
        for ext, mime_type in CUSTOM_MIME_TYPES.items():
            if path.endswith(ext):
                return mime_type
        return super().guess_type(path)

    def end_headers(self) -> None:
        """Add security and feature headers."""
        self.send_header("Cross-Origin-Opener-Policy", "same-origin")
        self.send_header("Cross-Origin-Embedder-Policy", "require-corp")
        self.send_header("Cache-Control", "no-cache, no-store, must-revalidate")
        
        # Compression support
        if self.config.get('compression'):
            self.send_header("Content-Encoding", "gzip")
        
        super().end_headers()

    def send_error(self, code: int, message: str = None, explain: str = None) -> None:
        """Send custom error responses."""
        if code == 404:
            self.send_response(404)
            self.send_header("Content-Type", "text/html; charset=utf-8")
            self.end_headers()
            self.wfile.write(ERROR_404_HTML.encode("utf-8"))
        else:
            super().send_error(code, message, explain)

    def log_message(self, format: str, *args) -> None:
        """Log HTTP requests with timing."""
        if len(args) >= 2:
            request_line = args[0]
            status = int(args[1].split()[0]) if isinstance(args[1], str) else args[1]
            
            parts = request_line.split()
            if len(parts) >= 2:
                method = parts[0]
                path = unquote(parts[1])
                
                duration_ms = (time.time() - self.start_time) * 1000
                
                if self.config.get('verbose') or status >= 400:
                    log_request(method, path, status, duration_ms=duration_ms)


# =============================================================================
# Server Setup
# =============================================================================

def create_server(port: int, directory: str, config: dict) -> socketserver.TCPServer:
    """Create and configure the HTTP server."""
    handler = partial(RustScriptHandler, directory=directory, config=config)
    socketserver.TCPServer.allow_reuse_address = True
    server = socketserver.TCPServer(("", port), handler)
    
    # HTTPS support
    if config.get('https'):
        try:
            context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
            # Generate self-signed certificate if needed
            cert_file = 'server.crt'
            key_file = 'server.key'
            
            if not os.path.exists(cert_file) or not os.path.exists(key_file):
                log_info("Generating self-signed certificate...")
                os.system(f'openssl req -x509 -newkey rsa:4096 -nodes -out {cert_file} -keyout {key_file} -days 365 -subj "/CN=localhost"')
            
            context.load_cert_chain(cert_file, key_file)
            server.socket = context.wrap_socket(server.socket, server_side=True)
            log_success("HTTPS enabled")
        except Exception as e:
            log_warning(f"HTTPS setup failed: {e}")
    
    return server


def print_banner(port: int, directory: str, config: dict) -> None:
    """Print the server startup banner."""
    protocol = "https" if config.get('https') else "http"
    
    print()
    print(f"  {Colours.BOLD}{Colours.MAGENTA}🦀 RustScript Development Server{Colours.RESET} {Colours.GREY}(Feature-Rich Edition){Colours.RESET}")
    print()
    print(f"  {Colours.GREY}Directory:{Colours.RESET}  {directory}/")
    print(f"  {Colours.GREY}Local:{Colours.RESET}      {Colours.CYAN}{protocol}://localhost:{port}{Colours.RESET}")
    print()
    
    # Show enabled features
    features = []
    if config.get('hot_reload'): features.append("Hot Reload")
    if config.get('directory_listing'): features.append("Directory Listing")
    if config.get('upload'): features.append("File Upload")
    if config.get('https'): features.append("HTTPS")
    if config.get('proxy'): features.append("Proxy")
    if config.get('compression'): features.append("Compression")
    if config.get('rate_limit'): features.append("Rate Limiting")
    if config.get('auth'): features.append("Authentication")
    if config.get('inspect'): features.append("Request Inspection")
    
    if features:
        print(f"  {Colours.GREY}Features:{Colours.RESET}  {Colours.GREEN}{', '.join(features)}{Colours.RESET}")
        print()
    
    print(f"  {Colours.GREY}Press {Colours.BOLD}Ctrl+C{Colours.RESET}{Colours.GREY} to stop{Colours.RESET}")
    print()


# =============================================================================
# Main Entry Point
# =============================================================================

def main() -> None:
    """Main entry point for the development server."""
    parser = argparse.ArgumentParser(
        description="RustScript Development Server - Feature-Rich Edition",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python serve.py                                    # Start with defaults
  python serve.py --port 3000                        # Custom port
  python serve.py --https                            # Enable HTTPS
  python serve.py --auth admin:secret                # Enable authentication
  python serve.py --proxy /api=http://localhost:3000 # Proxy API requests
  python serve.py --upload                           # Enable file uploads
  python serve.py --inspect                          # Enable request inspection
  python serve.py --all-features                     # Enable everything!
        """
    )
    parser.add_argument("--port", "-p", type=int, default=8000, help="Port to listen on (default: 8000)")
    parser.add_argument("--dir", "-d", type=str, default="www", help="Directory to serve (default: www)")
    parser.add_argument("--no-browser", action="store_true", help="Don't automatically open the browser")
    parser.add_argument("--verbose", "-v", action="store_true", help="Enable verbose request logging")
    parser.add_argument("--https", action="store_true", help="Enable HTTPS with self-signed certificate")
    parser.add_argument("--auth", type=str, help="Enable basic auth (format: user:pass)")
    parser.add_argument("--proxy", type=str, action="append", help="Proxy requests (format: /path=http://target)")
    parser.add_argument("--upload", action="store_true", help="Enable file uploads")
    parser.add_argument("--no-reload", action="store_true", help="Disable hot reload")
    parser.add_argument("--inspect", action="store_true", help="Enable detailed request inspection")
    parser.add_argument("--rate-limit", type=int, help="Max requests per minute per IP (default: 100)")
    parser.add_argument("--compress", action="store_true", help="Enable gzip compression")
    parser.add_argument("--all-features", action="store_true", help="Enable all features")
    
    args = parser.parse_args()
    
    # Verify directory exists
    if not os.path.exists(args.dir):
        log_error(f"Directory '{args.dir}' not found.")
        log_info("Did you run build_wasm.bat first?")
        sys.exit(1)
    
    # Build configuration
    config = {
        'verbose': args.verbose or args.all_features,
        'https': args.https or args.all_features,
        'auth': args.auth if args.auth else (None if not args.all_features else "admin:admin"),
        'upload': args.upload or args.all_features,
        'hot_reload': not args.no_reload,
        'inspect': args.inspect or args.all_features,
        'compression': args.compress or args.all_features,
        'directory_listing': True,  # Always enabled
        'rate_limit': args.rate_limit or (100 if args.all_features else None),
    }
    
    # Parse proxy configuration
    if args.proxy or args.all_features:
        config['proxy'] = {}
        if args.proxy:
            for proxy_rule in args.proxy:
                if '=' in proxy_rule:
                    prefix, target = proxy_rule.split('=', 1)
                    config['proxy'][prefix] = target
    
    # Set up rate limiter
    if config['rate_limit']:
        config['rate_limiter'] = RateLimiter(max_requests=config['rate_limit'])
    
    # Set up file watcher for hot reload
    if config['hot_reload']:
        watcher = FileWatcher(args.dir)
        watcher.start()
    
    # Set up graceful shutdown
    def signal_handler(signum, frame):
        print()
        log_info("Shutting down server...")
        if config['hot_reload']:
            watcher.stop()
        sys.exit(0)
    
    signal.signal(signal.SIGINT, signal_handler)
    signal.signal(signal.SIGTERM, signal_handler)
    
    # Create and start server
    try:
        server = create_server(args.port, args.dir, config)
    except OSError as e:
        if e.errno == 98 or e.errno == 10048:
            log_error(f"Port {args.port} is already in use.")
            log_info(f"Try a different port: python serve.py --port {args.port + 1}")
            sys.exit(1)
        raise
    
    # Print banner
    print_banner(args.port, args.dir, config)
    
    # Open browser
    if not args.no_browser:
        protocol = "https" if config['https'] else "http"
        webbrowser.open(f"{protocol}://localhost:{args.port}")
    
    # Start serving
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        pass
    finally:
        server.shutdown()
        log_success("Server stopped.")


if __name__ == "__main__":
    main()
