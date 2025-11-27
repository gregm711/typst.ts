use reflexo_typst2vec::ir::*;

macro_rules! layout {
    ($($prim:ty),* $(,)?) => {
        $(layout!(@one $prim);)*
    };
    (@one $prim:ty) => {{
        println!("const _: () = assert!(core::mem::size_of::<{}>() == {});", stringify!($prim), core::mem::size_of::<$prim>());
        println!("const _: () = assert!(core::mem::align_of::<{}>() == {});", stringify!($prim), core::mem::align_of::<$prim>());
    }};
}

fn main() {
    layout!(
        (),
        bool,
        u8,
        u16,
        u32,
        u64,
        i8,
        i16,
        i32,
        i64,
        f32,
        f64,
        char,
        ArchivedSourceMappingNode,
        ArchivedVecItem,
        ArchivedModuleMetadata,
        ArchivedTransformedRef,
        ArchivedGroupRef,
        ArchivedItemPack,
        ArchivedFlatModule,
        ArchivedDefId,
        ArchivedAbsoluteRef,
        ArchivedLinkItem,
        ArchivedPathItem,
        ArchivedTransformItem,
        ArchivedIncrGlyphPack,
        ArchivedPage,
        ArchivedBuildInfo,
        ArchivedRgba8Item,
        ArchivedColor32Item,
        ArchivedColorSpace,
        ArchivedGradientItem,
        ArchivedGradientKind,
        ArchivedGradientStyle,
        ArchivedLayoutRegionNode,
        ArchivedLayoutRegion,
        ArchivedLayoutSourceMapping,
        ArchivedImageGlyphItem,
        ArchivedOutlineGlyphItem,
        ArchivedFontItem,
        ArchivedTextShape,
        ArchivedTextItem,
        ArchivedTextItemContent,
        ArchivedFlatGlyphItem,
        ArchivedImageItem,
        ArchivedImage,
        ArchivedPathItem,
        ArchivedPathStyle,
        ArchivedPatternItem,
    );
}
