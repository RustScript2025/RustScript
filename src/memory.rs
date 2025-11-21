use std::alloc::{alloc, dealloc, Layout};
use std::mem;

// Expose memory allocation to the host environment (JavaScript)
// This allows the host to allocate memory within the WASM linear memory
// for passing strings, arrays, and other data structures.

#[no_mangle]
pub unsafe extern "C" fn malloc(size: usize) -> *mut u8 {
    let layout = Layout::from_size_align(size, mem::align_of::<u8>()).unwrap();
    alloc(layout)
}

#[no_mangle]
pub unsafe extern "C" fn free(ptr: *mut u8, size: usize) {
    let layout = Layout::from_size_align(size, mem::align_of::<u8>()).unwrap();
    dealloc(ptr, layout);
}
