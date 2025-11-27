pub mod escape;

pub use tinymist_std::*;
pub use tinymist_world::debug_loc;

pub mod vector {
    #[cfg(feature = "rkyv")]
    pub mod incr;
    pub mod ir;
    #[cfg(feature = "rkyv")]
    pub mod stream;
    pub mod vm;

    pub use ir::geom;

    #[cfg(feature = "rkyv")]
    #[allow(dead_code)]
    fn rkyv_assertions() {
        use ir::*;

        const _: () = assert!(core::mem::size_of::<()>() == 0);
        const _: () = assert!(core::mem::align_of::<()>() == 1);
        const _: () = assert!(core::mem::size_of::<bool>() == 1);
        const _: () = assert!(core::mem::align_of::<bool>() == 1);
        const _: () = assert!(core::mem::size_of::<u8>() == 1);
        const _: () = assert!(core::mem::align_of::<u8>() == 1);
        const _: () = assert!(core::mem::size_of::<u16>() == 2);
        const _: () = assert!(core::mem::align_of::<u16>() == 2);
        const _: () = assert!(core::mem::size_of::<u32>() == 4);
        const _: () = assert!(core::mem::align_of::<u32>() == 4);
        const _: () = assert!(core::mem::size_of::<u64>() == 8);
        const _: () = assert!(core::mem::align_of::<u64>() == 8);
        const _: () = assert!(core::mem::size_of::<i8>() == 1);
        const _: () = assert!(core::mem::align_of::<i8>() == 1);
        const _: () = assert!(core::mem::size_of::<i16>() == 2);
        const _: () = assert!(core::mem::align_of::<i16>() == 2);
        const _: () = assert!(core::mem::size_of::<i32>() == 4);
        const _: () = assert!(core::mem::align_of::<i32>() == 4);
        const _: () = assert!(core::mem::size_of::<i64>() == 8);
        const _: () = assert!(core::mem::align_of::<i64>() == 8);
        const _: () = assert!(core::mem::size_of::<f32>() == 4);
        const _: () = assert!(core::mem::align_of::<f32>() == 4);
        const _: () = assert!(core::mem::size_of::<f64>() == 8);
        const _: () = assert!(core::mem::align_of::<f64>() == 8);
        const _: () = assert!(core::mem::size_of::<char>() == 4);
        const _: () = assert!(core::mem::align_of::<char>() == 4);
        const _: () = assert!(core::mem::size_of::<ArchivedSourceMappingNode>() == 16);
        const _: () = assert!(core::mem::align_of::<ArchivedSourceMappingNode>() == 8);
        const _: () = assert!(core::mem::size_of::<ArchivedVecItem>() == 32);
        const _: () = assert!(core::mem::align_of::<ArchivedVecItem>() == 8);
        const _: () = assert!(core::mem::size_of::<ArchivedModuleMetadata>() == 12);
        const _: () = assert!(core::mem::align_of::<ArchivedModuleMetadata>() == 4);
        const _: () = assert!(core::mem::size_of::<ArchivedTransformedRef>() == 24);
        const _: () = assert!(core::mem::align_of::<ArchivedTransformedRef>() == 8);
        // const _: () = assert!(core::mem::size_of::<ArchivedGroupRef>() == 8);
        // const _: () = assert!(core::mem::align_of::<ArchivedGroupRef>() == 4);
        const _: () = assert!(core::mem::size_of::<ArchivedItemPack>() == 8);
        const _: () = assert!(core::mem::align_of::<ArchivedItemPack>() == 4);
// ...
        const _: () = assert!(core::mem::size_of::<ArchivedTextItem>() == 8);
        const _: () = assert!(core::mem::align_of::<ArchivedTextItem>() == 4);
        // const _: () = assert!(core::mem::size_of::<ArchivedTextItemContent>() == 16);
        // const _: () = assert!(core::mem::align_of::<ArchivedTextItemContent>() == 4);
        const _: () = assert!(core::mem::size_of::<ArchivedFlatGlyphItem>() == 8);
        const _: () = assert!(core::mem::align_of::<ArchivedFlatGlyphItem>() == 4);
        const _: () = assert!(core::mem::size_of::<ArchivedImageItem>() == 12);
        const _: () = assert!(core::mem::align_of::<ArchivedImageItem>() == 4);
        const _: () = assert!(core::mem::size_of::<ArchivedImage>() == 48);
        const _: () = assert!(core::mem::align_of::<ArchivedImage>() == 8);
        const _: () = assert!(core::mem::size_of::<ArchivedPathItem>() == 28);
        const _: () = assert!(core::mem::align_of::<ArchivedPathItem>() == 4);
        const _: () = assert!(core::mem::size_of::<ArchivedPathStyle>() == 12);
        const _: () = assert!(core::mem::align_of::<ArchivedPathStyle>() == 4);
        const _: () = assert!(core::mem::size_of::<ArchivedPatternItem>() == 32);
        const _: () = assert!(core::mem::align_of::<ArchivedPatternItem>() == 8);
    }
}
