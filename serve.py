import http.server
import socketserver
import os
import webbrowser

PORT = 8000
DIRECTORY = "www"

class Handler(http.server.SimpleHTTPRequestHandler):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, directory=DIRECTORY, **kwargs)

    def end_headers(self):
        # Enable Cross-Origin Opener Policy and Embedder Policy for SharedArrayBuffer support if needed
        self.send_header("Cross-Origin-Opener-Policy", "same-origin")
        self.send_header("Cross-Origin-Embedder-Policy", "require-corp")
        super().end_headers()

if __name__ == "__main__":
    # Change to the project root directory if needed
    if os.path.exists(DIRECTORY):
        print(f"Serving {DIRECTORY} at http://localhost:{PORT}")
        
        with socketserver.TCPServer(("", PORT), Handler) as httpd:
            print("Server started. Press Ctrl+C to stop.")
            webbrowser.open(f"http://localhost:{PORT}")
            try:
                httpd.serve_forever()
            except KeyboardInterrupt:
                print("\nServer stopped.")
    else:
        print(f"Error: Directory '{DIRECTORY}' not found. Did you run build_wasm.bat?")
