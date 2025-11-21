/**
 * RustScript Browser Loader
 * 
 * This script initializes the RustScript WASM runtime and automatically
 * compiles and executes any <script type="text/rustscript"> tags found in the DOM.
 */

import init, { run_script } from './pkg/RustScript.js';

async function bootstrap() {
    try {
        // Initialize WASM module
        await init();
        console.log("🦀 RustScript Runtime Initialized 🦀");

        // Find all RustScript tags
        const scripts = document.querySelectorAll('script[type="text/rustscript"]');

        for (const script of scripts) {
            if (script.src) {
                // Load external script
                await loadExternalScript(script.src);
            } else {
                // Execute inline script
                runInlineScript(script.textContent);
            }
        }

    } catch (e) {
        console.error("RustScript Bootstrap Error:", e);
    }
}

async function loadExternalScript(url) {
    try {
        const response = await fetch(url);
        if (!response.ok) throw new Error(`Failed to fetch ${url}: ${response.statusText}`);
        const source = await response.text();

        console.log(`Compiling and Running ${url}...`);
        await run_script(source);

    } catch (e) {
        console.error(`Error loading ${url}:`, e);
    }
}

async function runInlineScript(source) {
    try {
        console.log("Compiling and Running inline script...");
        await run_script(source);
    } catch (e) {
        console.error("Error executing inline script:", e);
    }
}

// Start the bootstrap process when the DOM is ready
if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', bootstrap);
} else {
    bootstrap();
}
