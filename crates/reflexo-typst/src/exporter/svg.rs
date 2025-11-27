use std::sync::Arc;
use std::{collections::HashSet, num::NonZeroU64};

use reflexo::error::prelude::*;

use reflexo::typst::Bytes;
use reflexo::typst::TypstPagedDocument;
use reflexo_vec2svg::{render_svg, render_svg_html, ExportFeature, SvgExporter};
use serde::{Deserialize, Serialize};
use serde_json;
use tinymist_task::{ExportSvgTask, ExportTask};
use tinymist_world::WorldDeps;
use typst::syntax::Span;
use typst::WorldExt;

use crate::world::{CompilerFeat, ExportComputation, WorldComputeGraph};

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct ExportWebSvgModuleTask {
    #[serde(flatten)]
    pub export: ExportTask,
}

pub struct WebSvgModuleExport<EF>(std::marker::PhantomData<EF>);

impl<EF: ExportFeature, F: CompilerFeat> ExportComputation<F, TypstPagedDocument>
    for WebSvgModuleExport<EF>
{
    type Output = Bytes;
    type Config = ExportWebSvgModuleTask;

    fn run(
        graph: &Arc<WorldComputeGraph<F>>,
        doc: &Arc<TypstPagedDocument>,
        _config: &Self::Config,
    ) -> Result<Bytes> {
        // Capture file table from the Typst world.
        let mut files: Vec<(u32, String)> = Vec::new();
        graph.snap.world.iter_dependencies(&mut |file_id| {
            if let Ok(resolved) = graph.snap.world.path_for_id(file_id) {
                let path_str = resolved.as_path().display().to_string();
                files.push((file_id.into_raw().get().into(), path_str));
            }
        });

        // Build vec doc first so spans are materialized.
        let mut vec_doc = SvgExporter::<EF>::svg_doc(doc);

        // Collect span ids to resolve ranges.
        let mut span_ids: HashSet<u64> = HashSet::new();
        for item in vec_doc.module.items.values() {
            match item {
                reflexo::vector::ir::VecItem::Group(g) => {
                    if let Some(s) = g.span_id {
                        span_ids.insert(s);
                    }
                }
                reflexo::vector::ir::VecItem::Text(t) => {
                    if let Some(s) = t.content.span_id {
                        span_ids.insert(s);
                    }
                }
                _ => {}
            }
        }

        let mut span_ranges: Vec<(u64, u32, usize, usize)> = Vec::new();
        for span_id in span_ids {
            if let Some(raw) = NonZeroU64::new(span_id) {
                let span = Span::from_raw(raw);
                let file_id = match span.id() {
                    Some(id) => id.into_raw().get().into(),
                    None => continue,
                };
                if let Some(range) = graph.snap.world.range(span) {
                    span_ranges.push((span_id, file_id, range.start, range.end));
                }
            }
        }

        if !files.is_empty() {
            let payload = serde_json::to_vec(&files).map_err(|e| {
                error_once!("serialize file table failed", err: e.to_string())
            })?;
            vec_doc.page_meta.push(reflexo::vector::ir::PageMetadata::Custom(vec![(
                "source_files".into(),
                payload.into(),
            )]));
        }
        if !span_ranges.is_empty() {
            let payload = serde_json::to_vec(&span_ranges).map_err(|e| {
                error_once!("serialize span ranges failed", err: e.to_string())
            })?;
            vec_doc.page_meta.push(reflexo::vector::ir::PageMetadata::Custom(vec![(
                "span_ranges".into(),
                payload.into(),
            )]));
        }

        Ok(Bytes::new(vec_doc.to_bytes()))
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct ExportWebSvgTask {
    #[serde(flatten)]
    pub base: ExportSvgTask,
}

pub struct WebSvgExport<EF>(std::marker::PhantomData<EF>);

impl<EF: ExportFeature, F: CompilerFeat> ExportComputation<F, TypstPagedDocument>
    for WebSvgExport<EF>
{
    type Output = String;
    type Config = ExportWebSvgTask;

    fn run(
        _g: &Arc<WorldComputeGraph<F>>,
        doc: &Arc<TypstPagedDocument>,
        _config: &Self::Config,
    ) -> Result<String> {
        Ok(render_svg(doc))
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct ExportWebSvgHtmlTask {
    #[serde(flatten)]
    pub base: ExportSvgTask,
}

pub struct WebSvgHtmlExport<EF>(std::marker::PhantomData<EF>);

impl<EF: ExportFeature, F: CompilerFeat> ExportComputation<F, TypstPagedDocument>
    for WebSvgHtmlExport<EF>
{
    type Output = String;
    type Config = ExportWebSvgHtmlTask;

    fn run(
        _g: &Arc<WorldComputeGraph<F>>,
        doc: &Arc<TypstPagedDocument>,
        _config: &Self::Config,
    ) -> Result<String> {
        Ok(render_svg_html::<EF>(doc))
    }
}
