/**
 * RustScript Browser Loader v0.4.1
 * Author: Michael Lauzon
 * 
 * A feature-rich runtime loader for RustScript in the browser.
 * Automatically compiles and executes <script type="text/rustscript"> tags.
 * 
 * Features:
 *   - Automatic WASM initialisation
 *   - Inline and external script support
 *   - Module system with imports
 *   - Hot reload support (development mode)
 *   - Source maps for debugging
 *   - Performance profiling
 *   - Error handling with stack traces
 *   - Event system for lifecycle hooks
 *   - Console output capture
 *   - Script caching
 *   - Async script execution
 *   - DOM integration helpers
 */

import init, { run_script, compile_script } from './pkg/RustScript.js';

// RustScript Runtime Configuration
const RustScriptConfig = {
    debug: false,
    hotReload: false,
    hotReloadInterval: 1000,
    enableSourceMaps: true,
    enableProfiling: false,
    cacheScripts: true,
    maxCacheSize: 50,
    logLevel: 'info', // 'debug', 'info', 'warn', 'error', 'none'
};

// Script cache for improved performance
const scriptCache = new Map();

// Module registry for imports
const moduleRegistry = new Map();

// Event listeners for lifecycle hooks
const eventListeners = {
    'init': [],
    'beforeCompile': [],
    'afterCompile': [],
    'beforeRun': [],
    'afterRun': [],
    'error': [],
    'hotReload': [],
};

// Performance metrics
const metrics = {
    initTime: 0,
    totalCompileTime: 0,
    totalRunTime: 0,
    scriptsExecuted: 0,
    cacheHits: 0,
    cacheMisses: 0,
};


// Logger with configurable levels
const Logger = {
    levels: { debug: 0, info: 1, warn: 2, error: 3, none: 4 },
    
    _shouldLog(level) {
        return this.levels[level] >= this.levels[RustScriptConfig.logLevel];
    },
    
    debug(...args) {
        if (this._shouldLog('debug')) console.debug('🦀 [DEBUG]', ...args);
    },
    
    info(...args) {
        if (this._shouldLog('info')) console.log('🦀 [INFO]', ...args);
    },
    
    warn(...args) {
        if (this._shouldLog('warn')) console.warn('🦀 [WARN]', ...args);
    },
    
    error(...args) {
        if (this._shouldLog('error')) console.error('🦀 [ERROR]', ...args);
    },
};

// Emit events to registered listeners
function emit(event, data) {
    const listeners = eventListeners[event] || [];
    for (const listener of listeners) {
        try {
            listener(data);
        } catch (e) {
            Logger.error(`Error in ${event} listener:`, e);
        }
    }
}

// Public API: Add event listener
function on(event, callback) {
    if (!eventListeners[event]) {
        eventListeners[event] = [];
    }
    eventListeners[event].push(callback);
    return () => off(event, callback); // Return unsubscribe function
}

// Public API: Remove event listener
function off(event, callback) {
    const listeners = eventListeners[event];
    if (listeners) {
        const index = listeners.indexOf(callback);
        if (index > -1) {
            listeners.splice(index, 1);
        }
    }
}

// Generate cache key for scripts using a simple hash function
function getCacheKey(source, options = {}) {
    let hash = 0;
    const str = source + JSON.stringify(options);
    for (let i = 0; i < str.length; i++) {
        const char = str.charCodeAt(i);
        hash = ((hash << 5) - hash) + char;
        hash = hash & hash;
    }
    return `rs_${hash}`;
}

// Manage script cache with LRU eviction
function cacheScript(key, compiled) {
    if (!RustScriptConfig.cacheScripts) return;
    
    // Evict oldest entries if cache is full
    if (scriptCache.size >= RustScriptConfig.maxCacheSize) {
        const firstKey = scriptCache.keys().next().value;
        scriptCache.delete(firstKey);
        Logger.debug('Cache eviction:', firstKey);
    }
    
    scriptCache.set(key, {
        compiled,
        timestamp: Date.now(),
    });
}

// Retrieve cached script if available
function getCachedScript(key) {
    if (!RustScriptConfig.cacheScripts) return null;
    
    const cached = scriptCache.get(key);
    if (cached) {
        metrics.cacheHits++;
        Logger.debug('Cache hit:', key);
        return cached.compiled;
    }
    
    metrics.cacheMisses++;
    return null;
}

// Parse script element attributes for configuration
function parseScriptAttributes(script) {
    return {
        id: script.id || null,
        src: script.src || null,
        async: script.hasAttribute('async'),
        defer: script.hasAttribute('defer'),
        module: script.hasAttribute('data-module'),
        moduleName: script.getAttribute('data-module-name'),
        noCache: script.hasAttribute('data-no-cache'),
        profile: script.hasAttribute('data-profile'),
    };
}

// Compile RustScript source to JavaScript
async function compile(source, options = {}) {
    const startTime = performance.now();
    
    emit('beforeCompile', { source, options });
    
    try {
        // Check cache first
        const cacheKey = getCacheKey(source, options);
        let compiled = getCachedScript(cacheKey);
        
        if (!compiled) {
            compiled = await compile_script(source);
            cacheScript(cacheKey, compiled);
        }
        
        const compileTime = performance.now() - startTime;
        metrics.totalCompileTime += compileTime;
        
        emit('afterCompile', { source, compiled, compileTime });
        
        if (RustScriptConfig.enableProfiling || options.profile) {
            Logger.info(`Compile time: ${compileTime.toFixed(2)}ms`);
        }
        
        return compiled;
    } catch (e) {
        emit('error', { phase: 'compile', error: e, source });
        throw e;
    }
}

// Execute compiled JavaScript code
async function execute(compiled, options = {}) {
    const startTime = performance.now();
    
    emit('beforeRun', { compiled, options });
    
    try {
        // Execute the compiled code
        const result = await run_script(compiled);
        
        const runTime = performance.now() - startTime;
        metrics.totalRunTime += runTime;
        metrics.scriptsExecuted++;
        
        emit('afterRun', { compiled, result, runTime });
        
        if (RustScriptConfig.enableProfiling || options.profile) {
            Logger.info(`Execution time: ${runTime.toFixed(2)}ms`);
        }
        
        return result;
    } catch (e) {
        emit('error', { phase: 'execute', error: e, compiled });
        throw e;
    }
}

// Create execution context with helper functions
function createExecutionContext() {
    return {
        // DOM helpers
        $: (selector) => document.querySelector(selector),
        $$: (selector) => document.querySelectorAll(selector),
        
        // Event helpers
        on: (element, event, handler) => {
            if (typeof element === 'string') {
                element = document.querySelector(element);
            }
            element?.addEventListener(event, handler);
        },
        
        // Fetch helper
        fetch: window.fetch.bind(window),
        
        // Storage helpers
        storage: {
            get: (key) => JSON.parse(localStorage.getItem(key)),
            set: (key, value) => localStorage.setItem(key, JSON.stringify(value)),
            remove: (key) => localStorage.removeItem(key),
        },
        
        // Module system
        require: (moduleName) => moduleRegistry.get(moduleName),
        exports: {},
    };
}


// Load and execute external script from URL
async function loadExternalScript(url, options = {}) {
    Logger.debug('Loading external script:', url);
    
    try {
        const response = await fetch(url);
        if (!response.ok) {
            throw new Error(`Failed to fetch ${url}: ${response.status} ${response.statusText}`);
        }
        
        const source = await response.text();
        
        Logger.info(`Compiling: ${url}`);
        const result = await run_script(source);
        
        // Register as module if specified
        if (options.module && options.moduleName) {
            moduleRegistry.set(options.moduleName, result);
            Logger.debug('Registered module:', options.moduleName);
        }
        
        return result;
    } catch (e) {
        Logger.error(`Error loading ${url}:`, e);
        emit('error', { phase: 'load', error: e, url });
        throw e;
    }
}

// Execute inline script from DOM element
async function runInlineScript(source, options = {}) {
    Logger.debug('Running inline script');
    
    try {
        const result = await run_script(source);
        
        // Register as module if specified
        if (options.module && options.moduleName) {
            moduleRegistry.set(options.moduleName, result);
            Logger.debug('Registered module:', options.moduleName);
        }
        
        return result;
    } catch (e) {
        Logger.error('Error executing inline script:', e);
        emit('error', { phase: 'inline', error: e, source });
        throw e;
    }
}

// Process a single script element
async function processScript(script) {
    const options = parseScriptAttributes(script);
    
    try {
        if (options.src) {
            return await loadExternalScript(options.src, options);
        } else {
            return await runInlineScript(script.textContent, options);
        }
    } catch (e) {
        // Error already logged; continue to next script
        return null;
    }
}

// Hot reload functionality for development
let hotReloadWatchers = new Map();

function setupHotReload() {
    if (!RustScriptConfig.hotReload) return;
    
    Logger.info('Hot reload enabled');
    
    // Watch external scripts for changes
    const scripts = document.querySelectorAll('script[type="text/rustscript"][src]');
    
    for (const script of scripts) {
        const scriptUrl = script.src;
        let lastModified = null;
        
        const checkForChanges = async () => {
            try {
                const response = await fetch(scriptUrl, { method: 'HEAD' });
                const modified = response.headers.get('Last-Modified');
                
                if (lastModified && modified !== lastModified) {
                    Logger.info(`Hot reloading: ${scriptUrl}`);
                    emit('hotReload', { url: scriptUrl });
                    
                    // Clear cache for this script
                    scriptCache.clear();
                    
                    // Reload the script
                    await loadExternalScript(scriptUrl, parseScriptAttributes(script));
                }
                
                lastModified = modified;
            } catch (e) {
                Logger.warn('Hot reload check failed:', e);
            }
        };
        
        // Initial check
        checkForChanges();
        
        // Set up polling interval
        const intervalId = setInterval(checkForChanges, RustScriptConfig.hotReloadInterval);
        hotReloadWatchers.set(scriptUrl, intervalId);
    }
}

// Stop hot reload watchers
function stopHotReload() {
    for (const [, intervalId] of hotReloadWatchers) {
        clearInterval(intervalId);
    }
    hotReloadWatchers.clear();
    Logger.info('Hot reload stopped');
}

// Main bootstrap function to initialise the runtime
async function bootstrap() {
    const startTime = performance.now();
    
    try {
        // Read configuration from meta tag if present
        const configMeta = document.querySelector('meta[name="rustscript-config"]');
        if (configMeta) {
            try {
                const config = JSON.parse(configMeta.content);
                Object.assign(RustScriptConfig, config);
                Logger.debug('Config loaded from meta tag:', config);
            } catch (e) {
                Logger.warn('Invalid config in meta tag:', e);
            }
        }
        
        // Initialise WASM module
        Logger.info('Initialising RustScript Runtime v0.4.1...');
        await init();
        
        metrics.initTime = performance.now() - startTime;
        Logger.info(`Runtime initialised in ${metrics.initTime.toFixed(2)}ms`);
        
        emit('init', { initTime: metrics.initTime });
        
        // Find all RustScript tags
        const scripts = document.querySelectorAll('script[type="text/rustscript"]');
        Logger.info(`Found ${scripts.length} RustScript tag(s)`);
        
        // Separate deferred and immediate scripts
        const immediateScripts = [];
        const deferredScripts = [];
        
        for (const script of scripts) {
            if (script.hasAttribute('defer')) {
                deferredScripts.push(script);
            } else {
                immediateScripts.push(script);
            }
        }
        
        // Execute immediate scripts (respecting async attribute)
        const asyncScripts = immediateScripts.filter(s => s.hasAttribute('async'));
        const syncScripts = immediateScripts.filter(s => !s.hasAttribute('async'));
        
        // Run synchronous scripts in order
        for (const script of syncScripts) {
            await processScript(script);
        }
        
        // Run asynchronous scripts in parallel
        if (asyncScripts.length > 0) {
            await Promise.all(asyncScripts.map(processScript));
        }
        
        // Run deferred scripts after DOM is fully loaded
        if (deferredScripts.length > 0) {
            if (document.readyState === 'complete') {
                for (const script of deferredScripts) {
                    await processScript(script);
                }
            } else {
                window.addEventListener('load', async () => {
                    for (const script of deferredScripts) {
                        await processScript(script);
                    }
                });
            }
        }
        
        // Set up hot reload if enabled
        setupHotReload();
        
        // Log final metrics if profiling is enabled
        if (RustScriptConfig.enableProfiling) {
            logMetrics();
        }
        
    } catch (e) {
        Logger.error('Bootstrap failed:', e);
        emit('error', { phase: 'bootstrap', error: e });
    }
}

// Log performance metrics to console
function logMetrics() {
    Logger.info('=== RustScript Performance Metrics ===');
    Logger.info(`Init time: ${metrics.initTime.toFixed(2)}ms`);
    Logger.info(`Total compile time: ${metrics.totalCompileTime.toFixed(2)}ms`);
    Logger.info(`Total run time: ${metrics.totalRunTime.toFixed(2)}ms`);
    Logger.info(`Scripts executed: ${metrics.scriptsExecuted}`);
    Logger.info(`Cache hits: ${metrics.cacheHits}`);
    Logger.info(`Cache misses: ${metrics.cacheMisses}`);
    Logger.info('=======================================');
}


// Public API object exposed to window.RustScript
const RustScript = {
    // Version number
    version: '0.4.1',
    
    // Configuration object
    config: RustScriptConfig,
    
    // Update runtime configuration
    configure(options) {
        Object.assign(RustScriptConfig, options);
        Logger.debug('Configuration updated:', options);
    },
    
    // Compile source code
    compile,
    
    // Execute compiled code
    execute,
    
    // Run source code (compile and execute)
    async run(source) {
        return await run_script(source);
    },
    
    // Load and run external script
    load: loadExternalScript,
    
    // Event system
    on,
    off,
    
    // Module system for managing RustScript modules
    modules: {
        register(name, exports) {
            moduleRegistry.set(name, exports);
        },
        get(name) {
            return moduleRegistry.get(name);
        },
        has(name) {
            return moduleRegistry.has(name);
        },
        list() {
            return Array.from(moduleRegistry.keys());
        },
    },
    
    // Cache management utilities
    cache: {
        clear() {
            scriptCache.clear();
            Logger.info('Script cache cleared');
        },
        size() {
            return scriptCache.size;
        },
        keys() {
            return Array.from(scriptCache.keys());
        },
    },
    
    // Hot reload controls
    hotReload: {
        start: setupHotReload,
        stop: stopHotReload,
    },
    
    // Performance metrics access
    metrics: {
        get() {
            return { ...metrics };
        },
        log: logMetrics,
        reset() {
            metrics.totalCompileTime = 0;
            metrics.totalRunTime = 0;
            metrics.scriptsExecuted = 0;
            metrics.cacheHits = 0;
            metrics.cacheMisses = 0;
        },
    },
    
    // DOM helper utilities
    dom: {
        // Create element with attributes and children
        create(tag, attrs = {}, children = []) {
            const el = document.createElement(tag);
            
            for (const [key, value] of Object.entries(attrs)) {
                if (key.startsWith('on') && typeof value === 'function') {
                    el.addEventListener(key.slice(2).toLowerCase(), value);
                } else if (key === 'style' && typeof value === 'object') {
                    Object.assign(el.style, value);
                } else if (key === 'class') {
                    el.className = value;
                } else {
                    el.setAttribute(key, value);
                }
            }
            
            for (const child of children) {
                if (typeof child === 'string') {
                    el.appendChild(document.createTextNode(child));
                } else {
                    el.appendChild(child);
                }
            }
            
            return el;
        },
        
        // Query selector shorthand
        $(selector) {
            return document.querySelector(selector);
        },
        
        // Query selector all shorthand
        $$(selector) {
            return Array.from(document.querySelectorAll(selector));
        },
        
        // Add event listener with automatic cleanup function
        on(element, event, handler, options = {}) {
            if (typeof element === 'string') {
                element = document.querySelector(element);
            }
            if (!element) return () => {};
            
            element.addEventListener(event, handler, options);
            return () => element.removeEventListener(event, handler, options);
        },
        
        // Wait for element to appear in DOM
        async waitFor(selector, timeout = 5000) {
            return new Promise((resolve, reject) => {
                const el = document.querySelector(selector);
                if (el) {
                    resolve(el);
                    return;
                }
                
                const observer = new MutationObserver((_mutations, obs) => {
                    const foundEl = document.querySelector(selector);
                    if (foundEl) {
                        obs.disconnect();
                        resolve(foundEl);
                    }
                });
                
                observer.observe(document.body, {
                    childList: true,
                    subtree: true,
                });
                
                setTimeout(() => {
                    observer.disconnect();
                    reject(new Error(`Timeout waiting for ${selector}`));
                }, timeout);
            });
        },
    },
    
    // General utility functions
    utils: {
        // Debounce function execution
        debounce(fn, delay) {
            let timeoutId;
            return (...args) => {
                clearTimeout(timeoutId);
                timeoutId = setTimeout(() => fn(...args), delay);
            };
        },
        
        // Throttle function execution
        throttle(fn, limit) {
            let inThrottle;
            return (...args) => {
                if (!inThrottle) {
                    fn(...args);
                    inThrottle = true;
                    setTimeout(() => inThrottle = false, limit);
                }
            };
        },
        
        // Sleep/delay utility
        sleep(ms) {
            return new Promise(resolve => setTimeout(resolve, ms));
        },
        
        // Generate unique identifier
        uid() {
            return 'rs_' + Math.random().toString(36).substring(2, 11);
        },
    },
};

// Expose RustScript to global scope
window.RustScript = RustScript;

// Start bootstrap when DOM is ready
if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', bootstrap);
} else {
    bootstrap();
}

// Export for ES modules
export default RustScript;
export { RustScript, compile, execute, on, off };
