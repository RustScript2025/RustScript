//! Memory management for WebAssembly.
//! 
//! Author: Michael Lauzon
//! 
//! This module provides malloc/free implementations that can be called from
//! WebAssembly code. These functions manage the linear memory heap and are
//! essential for dynamic allocation of strings, structs, and other data.
//! 
//! The allocator uses Rust's global allocator, which provides efficient
//! memory management with minimal overhead.

use std::alloc::{alloc, dealloc, Layout};
use std::mem;

/// Allocates memory in the WebAssembly linear memory heap.
/// 
/// This function is exported to WebAssembly and can be called from generated
/// code to allocate memory for structs, strings, and other dynamic data.
/// 
/// # Safety
/// 
/// This function is unsafe because it returns a raw pointer. The caller must
/// ensure the pointer is used correctly and eventually freed with `free`.
/// 
/// # Arguments
/// 
/// * `size` - The number of bytes to allocate
/// 
/// # Returns
/// 
/// A pointer to the allocated memory, or null if allocation fails
#[unsafe(no_mangle)]
pub unsafe extern "C" fn malloc(size: usize) -> *mut u8 {
    if size == 0 {
        return std::ptr::null_mut();
    }
    
    let layout = match Layout::from_size_align(size, mem::align_of::<u8>()) {
        Ok(layout) => layout,
        Err(_) => return std::ptr::null_mut(),
    };
    
    // SAFETY: Layout is valid (checked above), and we return null on failure
    unsafe { alloc(layout) }
}

/// Frees memory previously allocated with `malloc`.
/// 
/// # Safety
/// 
/// This function is unsafe because it deallocates memory. The caller must ensure:
/// - The pointer was allocated with `malloc`
/// - The size matches the original allocation
/// - The pointer is not used after being freed
/// 
/// # Arguments
/// 
/// * `ptr` - Pointer to the memory to free
/// * `size` - Size of the allocation (must match the original malloc call)
#[unsafe(no_mangle)]
pub unsafe extern "C" fn free(ptr: *mut u8, size: usize) {
    if ptr.is_null() || size == 0 {
        return;
    }
    
    let layout = match Layout::from_size_align(size, mem::align_of::<u8>()) {
        Ok(layout) => layout,
        Err(_) => return,
    };
    
    // SAFETY: ptr was allocated with the same layout by malloc
    unsafe { dealloc(ptr, layout) };
}
