#!/usr/bin/env python3
"""
RustScript Development Server
Author: Michael Lauzon

A feature-rich HTTP server for testing RustScript programmes in the browser.
This server serves the www/ directory and adds the necessary CORS headers
for WebAssembly modules that require SharedArrayBuffer support.

Features:
    - Custom port and directory via command-line arguments
    - Proper MIME types for .wasm and .rjsc files
    - Coloured console output for better readability
    - Request logging with timestamps
    - Custom 404 error page
    - Graceful shutdown handling
    - Optional auto-open browser

Usage:
    python serve.py                     # Default: port 8000, www/ directory
    python serve.py --port 3000         # Custom port
    python serve.py --dir public        # Custom directory
    python serve.py --no-browser        # Don't auto-open browser
    python serve.py --verbose           # Enable verbose logging

Press Ctrl+C to stop the server.
"""

import http.server
import socketserver
import os
import sys
import argparse
import webbrowser
import signal
from datetime import datetime
from functools import partial
from urllib.parse import unquote


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
        # Windows requires special handling for ANSI colours.
        if sys.platform == "win32":
            try:
                import ctypes
                kernel32 = ctypes.windll.kernel32
                # Enable ANSI escape sequences on Windows 10+.
                kernel32.SetConsoleMode(
                    kernel32.GetStdHandle(-11), 7
                )
                return True
            except Exception:
                return False
        # Most Unix terminals support colour.
        return hasattr(sys.stdout, "isatty") and sys.stdout.isatty()


# Disable colours if the terminal doesn't support them.
if not Colours.supports_colour():
    for attr in dir(Colours):
        if not attr.startswith("_") and attr != "supports_colour":
            setattr(Colours, attr, "")


# =============================================================================
# Custom MIME Types
# =============================================================================

# Additional MIME types for RustScript and WebAssembly files.
CUSTOM_MIME_TYPES = {
    ".wasm": "application/wasm",
    ".rjsc": "text/plain",
    ".mjs": "application/javascript",
    ".map": "application/json",
}


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
    print(f"{Colours.GREY}[{timestamp}]{Colours.RESET} {Colours.GREEN}{message}{Colours.RESET}")


def log_warning(message: str) -> None:
    """Log a warning message in yellow."""
    timestamp = datetime.now().strftime("%H:%M:%S")
    print(f"{Colours.GREY}[{timestamp}]{Colours.RESET} {Colours.YELLOW}⚠ {message}{Colours.RESET}")


def log_error(message: str) -> None:
    """Log an error message in red."""
    timestamp = datetime.now().strftime("%H:%M:%S")
    print(f"{Colours.GREY}[{timestamp}]{Colours.RESET} {Colours.RED}✗ {message}{Colours.RESET}")


def log_request(method: str, path: str, status: int, size: int = 0) -> None:
    """Log an HTTP request with colour-coded status."""
    timestamp = datetime.now().strftime("%H:%M:%S")
    
    # Colour-code the status based on its category.
    if 200 <= status < 300:
        status_colour = Colours.GREEN
    elif 300 <= status < 400:
        status_colour = Colours.CYAN
    elif 400 <= status < 500:
        status_colour = Colours.YELLOW
    else:
        status_colour = Colours.RED
    
    # Format the size for display.
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
    
    print(
        f"{Colours.GREY}[{timestamp}]{Colours.RESET} "
        f"{Colours.BOLD}{method}{Colours.RESET} {path} "
        f"{status_colour}{status}{Colours.RESET}{size_display}"
    )


# =============================================================================
# Custom 404 Error Page
# =============================================================================

ERROR_404_HTML = """<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>404 - Not Found</title>
    <style>
        body {
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
            background: #1a1a2e;
            color: #eee;
            display: flex;
            justify-content: center;
            align-items: center;
            min-height: 100vh;
            margin: 0;
        }
        .container {
            text-align: center;
        }
        h1 {
            color: #e94560;
            font-size: 4rem;
            margin: 0;
        }
        p {
            color: #888;
            font-size: 1.2rem;
        }
        a {
            color: #e94560;
            text-decoration: none;
        }
        a:hover {
            text-decoration: underline;
        }
        .emoji {
            font-size: 5rem;
            margin-bottom: 1rem;
        }
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
</html>
"""


# =============================================================================
# Request Handler
# =============================================================================

class RustScriptHandler(http.server.SimpleHTTPRequestHandler):
    """
    Custom HTTP request handler for the RustScript development server.
    
    This handler extends SimpleHTTPRequestHandler with:
    - Cross-Origin headers for WebAssembly support
    - Custom MIME types for .wasm and .rjsc files
    - Request logging with timestamps
    - Custom 404 error page
    """

    def __init__(self, *args, directory: str = None, verbose: bool = False, **kwargs):
        """
        Initialise the handler with the specified directory.
        
        Args:
            directory: The directory to serve files from.
            verbose: Whether to enable verbose logging.
        """
        self.verbose = verbose
        super().__init__(*args, directory=directory, **kwargs)

    def guess_type(self, path: str) -> str:
        """
        Determine the MIME type for a file.
        
        This method extends the default behaviour to include custom MIME types
        for WebAssembly and RustScript files.
        
        Args:
            path: The file path to determine the type for.
            
        Returns:
            The MIME type string.
        """
        # Check for custom MIME types first.
        for ext, mime_type in CUSTOM_MIME_TYPES.items():
            if path.endswith(ext):
                return mime_type
        # Fall back to the default implementation.
        return super().guess_type(path)

    def end_headers(self) -> None:
        """
        Add security headers before sending the response.
        
        These headers are required for WebAssembly features like SharedArrayBuffer,
        which some browsers restrict for security reasons.
        """
        # Cross-Origin-Opener-Policy isolates the browsing context.
        self.send_header("Cross-Origin-Opener-Policy", "same-origin")
        # Cross-Origin-Embedder-Policy requires all resources to be CORS-enabled.
        self.send_header("Cross-Origin-Embedder-Policy", "require-corp")
        # Cache control for development (disable caching).
        self.send_header("Cache-Control", "no-cache, no-store, must-revalidate")
        super().end_headers()

    def send_error(self, code: int, message: str = None, explain: str = None) -> None:
        """
        Send a custom error response.
        
        For 404 errors, this sends a styled HTML page instead of the default.
        
        Args:
            code: The HTTP status code.
            message: A short error message.
            explain: A longer explanation of the error.
        """
        if code == 404:
            self.send_response(404)
            self.send_header("Content-Type", "text/html; charset=utf-8")
            self.end_headers()
            self.wfile.write(ERROR_404_HTML.encode("utf-8"))
        else:
            super().send_error(code, message, explain)

    def log_message(self, format: str, *args) -> None:
        """
        Log an HTTP request.
        
        This overrides the default logging to use our custom coloured output.
        
        Args:
            format: The log format string.
            args: The format arguments (typically status code, path, etc.).
        """
        # Parse the request details from the format arguments.
        if len(args) >= 2:
            # args[0] is typically "GET /path HTTP/1.1"
            # args[1] is the status code
            request_line = args[0]
            status = int(args[1].split()[0]) if isinstance(args[1], str) else args[1]
            
            # Extract method and path from the request line.
            parts = request_line.split()
            if len(parts) >= 2:
                method = parts[0]
                path = unquote(parts[1])
                
                # Only log if verbose mode is enabled or it's not a successful request.
                if self.verbose or status >= 400:
                    log_request(method, path, status)
                elif status < 400:
                    # In non-verbose mode, still log but more quietly.
                    log_request(method, path, status)


# =============================================================================
# Server Setup and Main Entry Point
# =============================================================================

def create_server(port: int, directory: str, verbose: bool) -> socketserver.TCPServer:
    """
    Create and configure the HTTP server.
    
    Args:
        port: The port number to listen on.
        directory: The directory to serve files from.
        verbose: Whether to enable verbose logging.
        
    Returns:
        A configured TCPServer instance.
    """
    # Create a handler class with the directory and verbose settings bound.
    handler = partial(RustScriptHandler, directory=directory, verbose=verbose)
    
    # Allow address reuse to avoid "Address already in use" errors.
    socketserver.TCPServer.allow_reuse_address = True
    
    return socketserver.TCPServer(("", port), handler)


def print_banner(port: int, directory: str) -> None:
    """Print the server startup banner."""
    print()
    print(f"  {Colours.BOLD}{Colours.MAGENTA}🦀 RustScript Development Server{Colours.RESET}")
    print()
    print(f"  {Colours.GREY}Directory:{Colours.RESET}  {directory}/")
    print(f"  {Colours.GREY}Local:{Colours.RESET}      {Colours.CYAN}http://localhost:{port}{Colours.RESET}")
    print()
    print(f"  {Colours.GREY}Press {Colours.BOLD}Ctrl+C{Colours.RESET}{Colours.GREY} to stop{Colours.RESET}")
    print()


def main() -> None:
    """Main entry point for the development server."""
    # Parse command-line arguments.
    parser = argparse.ArgumentParser(
        description="RustScript Development Server",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python serve.py                     Start with defaults (port 8000, www/)
  python serve.py --port 3000         Use a custom port
  python serve.py --dir public        Serve a different directory
  python serve.py --no-browser        Don't open the browser automatically
  python serve.py --verbose           Enable verbose request logging
        """
    )
    parser.add_argument(
        "--port", "-p",
        type=int,
        default=8000,
        help="Port to listen on (default: 8000)"
    )
    parser.add_argument(
        "--dir", "-d",
        type=str,
        default="www",
        help="Directory to serve (default: www)"
    )
    parser.add_argument(
        "--no-browser",
        action="store_true",
        help="Don't automatically open the browser"
    )
    parser.add_argument(
        "--verbose", "-v",
        action="store_true",
        help="Enable verbose request logging"
    )
    
    args = parser.parse_args()
    
    # Verify the directory exists.
    if not os.path.exists(args.dir):
        log_error(f"Directory '{args.dir}' not found.")
        log_info("Did you run build_wasm.bat first?")
        sys.exit(1)
    
    # Set up graceful shutdown handling.
    def signal_handler(signum, frame):
        """Handle shutdown signals gracefully."""
        print()
        log_info("Shutting down server...")
        sys.exit(0)
    
    signal.signal(signal.SIGINT, signal_handler)
    signal.signal(signal.SIGTERM, signal_handler)
    
    # Create and start the server.
    try:
        server = create_server(args.port, args.dir, args.verbose)
    except OSError as e:
        if e.errno == 98 or e.errno == 10048:  # Address already in use.
            log_error(f"Port {args.port} is already in use.")
            log_info(f"Try a different port: python serve.py --port {args.port + 1}")
            sys.exit(1)
        raise
    
    # Print the startup banner.
    print_banner(args.port, args.dir)
    
    # Open the browser if requested.
    if not args.no_browser:
        webbrowser.open(f"http://localhost:{args.port}")
    
    # Start serving requests.
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        pass
    finally:
        server.shutdown()
        log_success("Server stopped.")


if __name__ == "__main__":
    main()
