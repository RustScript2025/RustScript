# RustScript Development Server

**Author**: Michael Lauzon

A feature-rich HTTP development server for testing RustScript programmes in the browser.

## Quick Start

**On Windows:**
```powershell
# Basic usage (serves www/ directory on port 8000)
python serve.py

# Enable all features
python serve.py --all-features

# Custom port and directory
python serve.py --port 3000 --dir public
```

**On Linux/Mac:**
```bash
# Basic usage (serves www/ directory on port 8000)
python3 serve.py

# Enable all features
python3 serve.py --all-features

# Custom port and directory
python3 serve.py --port 3000 --dir public
```

## Features

The RustScript development server includes 10 powerful features:

### 1. 🔄 Hot Reload

Automatically refreshes your browser when files change. Perfect for rapid development.

```bash
python serve.py  # Hot reload enabled by default
python serve.py --no-reload  # Disable if needed
```

**Watches:** `.html`, `.js`, `.css`, `.rscc`, `.wasm` files

### 2. 📁 Directory Listing

Beautiful, styled directory listings with file icons and sizes.

**Features:**
- Folder and file icons
- File size display
- Sorted alphabetically
- Dark theme matching RustScript style

### 3. 📤 File Upload

Upload files directly through the browser interface.

```bash
python serve.py --upload
```

**Usage:**
1. Navigate to any directory
2. Use the upload form at the bottom
3. Select and upload files

### 4. 🔒 HTTPS Support

Serve over HTTPS with automatically generated self-signed certificates.

```bash
python serve.py --https
```

**First run:** Generates `server.crt` and `server.key`  
**Access:** `https://localhost:8000`  
**Note:** Browser will warn about self-signed certificate (this is normal for development)

### 5. 🔀 Proxy Support

Proxy API requests to another server. Perfect for full-stack development.

```bash
# Proxy /api requests to backend server
python serve.py --proxy /api=http://localhost:3000

# Multiple proxies
python serve.py --proxy /api=http://localhost:3000 --proxy /auth=http://localhost:4000
```

**Example:**
- Request to `http://localhost:8000/api/users`
- Proxied to `http://localhost:3000/users`

### 6. 🗜️ Compression

Gzip compression for faster page loads.

```bash
python serve.py --compress
```

**Benefits:**
- Smaller file sizes
- Faster load times
- Reduced bandwidth usage

### 7. 🚦 Rate Limiting

Prevent abuse by limiting requests per IP address.

```bash
# Limit to 50 requests per minute per IP
python serve.py --rate-limit 50

# Default is 100 requests per minute
python serve.py --all-features
```

**Response:** Returns `429 Too Many Requests` when limit exceeded

### 8. 🔐 Basic Authentication

Password protect your development server.

```bash
# Set username and password
python serve.py --auth admin:secret123

# With all features (default: admin:admin)
python serve.py --all-features
```

**Access:** Browser will prompt for credentials

### 9. 🔌 WebSocket Support

Infrastructure for real-time features (used by hot reload).

**Automatic:** Enabled when hot reload is active

### 10. 🔍 Request Inspection

Detailed logging of all HTTP requests.

```bash
python serve.py --inspect
```

**Logs:**
- Request method and path
- Client IP and port
- All request headers
- Response timing

## Command-Line Options

### Basic Options

| Option | Short | Default | Description |
|--------|-------|---------|-------------|
| `--port` | `-p` | `8000` | Port to listen on |
| `--dir` | `-d` | `www` | Directory to serve |
| `--no-browser` | | | Don't auto-open browser |
| `--verbose` | `-v` | | Enable verbose logging |

### Feature Options

| Option | Description |
|--------|-------------|
| `--https` | Enable HTTPS with self-signed certificate |
| `--auth USER:PASS` | Enable basic authentication |
| `--proxy /path=http://target` | Proxy requests to another server |
| `--upload` | Enable file uploads |
| `--no-reload` | Disable hot reload |
| `--inspect` | Enable detailed request inspection |
| `--rate-limit N` | Max requests per minute per IP |
| `--compress` | Enable gzip compression |
| `--all-features` | Enable all features at once |

## Usage Examples

### Development Workflow

```bash
# Standard development
python serve.py

# With verbose logging
python serve.py --verbose

# Custom port (if 8000 is busy)
python serve.py --port 3001
```

### Full-Stack Development

```bash
# Frontend on 8000, proxy API to backend on 3000
python serve.py --proxy /api=http://localhost:3000
```

### Secure Development

```bash
# HTTPS with authentication
python serve.py --https --auth dev:password123
```

### Testing File Uploads

```bash
# Enable uploads and inspection
python serve.py --upload --inspect
```

### Production-Like Testing

```bash
# All security and performance features
python serve.py --https --auth admin:secret --rate-limit 50 --compress
```

### Everything Enabled

```bash
# Enable all features with one flag
python serve.py --all-features
```

## Console Output

The server provides coloured, timestamped console output:

```
  🦀 RustScript Development Server (Feature-Rich Edition)

  Directory:  www/
  Local:      http://localhost:8000

  Features:  Hot Reload, Directory Listing, File Upload, Compression

  Press Ctrl+C to stop

[14:23:45] GET /index.html 200 (2.3 KB) 15ms
[14:23:46] GET /styles.css 200 (1.1 KB) 8ms
[14:23:47] ✓ File uploaded: example.rscc
```

### Colour Coding

- **Green**: Successful requests (2xx)
- **Cyan**: Redirects (3xx)
- **Yellow**: Client errors (4xx)
- **Red**: Server errors (5xx)
- **Grey**: Timestamps and metadata

## MIME Types

The server automatically serves correct MIME types for RustScript files:

| Extension | MIME Type | Description |
|-----------|-----------|-------------|
| `.rscc` | `text/plain` | RustScript source code |
| `.rscx` | `application/octet-stream` | RustScript executable |
| `.wasm` | `application/wasm` | WebAssembly binary |
| `.mjs` | `application/javascript` | JavaScript module |
| `.map` | `application/json` | Source map |

## Security Headers

The server automatically adds security headers required for WebAssembly:

```
Cross-Origin-Opener-Policy: same-origin
Cross-Origin-Embedder-Policy: require-corp
Cache-Control: no-cache, no-store, must-revalidate
```

These headers enable:
- SharedArrayBuffer support
- WebAssembly threading
- Proper CORS handling

## Troubleshooting

### Port Already in Use

```bash
# Error: Port 8000 is already in use
# Solution: Use a different port
python serve.py --port 8001
```

### Directory Not Found

```
Error: Directory 'www' not found
```

**Solution**: Build the WASM package first:

**On Windows:**
```powershell
build_wasm.bat
```

**On Linux/Mac:**
```bash
./build_wasm.sh
```

### HTTPS Certificate Warnings

**This is normal for self-signed certificates in development.**

To proceed:
- Chrome: Click "Advanced" → "Proceed to localhost"
- Firefox: Click "Advanced" → "Accept the Risk"
- Safari: Click "Show Details" → "Visit Website"

### Hot Reload Not Working

1. Check that files are in the watched directory
2. Ensure file extensions are supported (`.html`, `.js`, `.css`, `.rscc`, `.wasm`)
3. Try hard refresh in browser (Ctrl+Shift+R or Cmd+Shift+R)

### Upload Fails

1. Ensure `--upload` flag is enabled
2. Check directory permissions
3. Verify file size isn't too large

### Proxy Not Working

1. Verify backend server is running
2. Check proxy path matches exactly
3. Ensure no trailing slashes in configuration

## Advanced Configuration

### Multiple Proxies

```bash
python serve.py \
  --proxy /api=http://localhost:3000 \
  --proxy /auth=http://localhost:4000 \
  --proxy /ws=http://localhost:5000
```

### Custom Rate Limiting

```bash
# Very strict (10 requests/minute)
python serve.py --rate-limit 10

# Relaxed (500 requests/minute)
python serve.py --rate-limit 500
```

### Combining Features

```bash
# Full-featured development server
python serve.py \
  --port 8080 \
  --https \
  --auth dev:secret \
  --upload \
  --compress \
  --rate-limit 100 \
  --inspect \
  --verbose
```

## Performance Tips

1. **Use compression** for faster page loads: `--compress`
2. **Disable hot reload** for large projects: `--no-reload`
3. **Set appropriate rate limits** to prevent abuse
4. **Use proxy** instead of CORS for API calls

## Integration with Build Tools

### With Cargo Watch

```bash
# Terminal 1: Watch and rebuild
cargo watch -x 'build --release'

# Terminal 2: Serve with hot reload
python serve.py
```

### With npm/yarn

```bash
# Terminal 1: Build frontend
npm run watch

# Terminal 2: Serve
python serve.py --proxy /api=http://localhost:3000
```

## Requirements

- Python 3.6+
- OpenSSL (for HTTPS certificate generation)

**Optional:**
- Modern browser with WebAssembly support
- Node.js (for running compiled JavaScript)

## See Also

- [README.md](../README.md) - Project overview
- [TUTORIAL.md](TUTORIAL.md) - Complete tutorial
- [CONTRIBUTING.md](../CONTRIBUTING.md) - Development guide

## Getting Help

```bash
# Show all options
python serve.py --help

# Check Python version
python --version

# Test basic functionality
python serve.py --verbose
```

---

**Questions or issues?** Open an issue on GitHub or check the documentation.
