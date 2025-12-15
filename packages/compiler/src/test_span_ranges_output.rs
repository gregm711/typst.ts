#[cfg(all(test, feature = "incr", not(target_arch = "wasm32")))]
mod tests {
    use std::collections::{HashMap, HashSet};
    use std::num::NonZeroU64;

    use typst::diag::{FileError, FileResult};
    use typst::foundations::{Bytes, Datetime};
    use typst::syntax::{FileId, Source, Span, VirtualPath};
    use typst::text::{Font, FontBook};
    use typst::utils::LazyHash;
    use typst::{Library, LibraryExt, World};

    struct TestWorld {
        library: LazyHash<Library>,
        book: LazyHash<FontBook>,
        main: FileId,
        sources: HashMap<FileId, Source>,
    }

    impl TestWorld {
        fn new(main: FileId, sources: HashMap<FileId, Source>) -> Self {
            Self {
                library: LazyHash::new(Library::default()),
                book: LazyHash::new(FontBook::default()),
                main,
                sources,
            }
        }
    }

    impl World for TestWorld {
        fn library(&self) -> &LazyHash<Library> {
            &self.library
        }

        fn book(&self) -> &LazyHash<FontBook> {
            &self.book
        }

        fn main(&self) -> FileId {
            self.main
        }

        fn source(&self, id: FileId) -> FileResult<Source> {
            self.sources.get(&id).cloned().ok_or_else(|| {
                FileError::NotFound(id.vpath().as_rooted_path().to_path_buf())
            })
        }

        fn file(&self, id: FileId) -> FileResult<Bytes> {
            Err(FileError::NotFound(id.vpath().as_rooted_path().to_path_buf()))
        }

        fn font(&self, _index: usize) -> Option<Font> {
            None
        }

        fn today(&self, _offset: Option<i64>) -> Option<Datetime> {
            None
        }
    }

    #[test]
    fn resolves_span_ranges_and_source_files_across_files() {
        let main_id = FileId::new(None, VirtualPath::new("/main.typ"));
        let chap_id = FileId::new(None, VirtualPath::new("/chap1.typ"));
        let missing_id = FileId::new(None, VirtualPath::new("/missing.typ"));

        let source_main = Source::new(main_id, "Hello world".to_string());
        let source_chap = Source::new(chap_id, "Chapter".to_string());

        // Create a numbered span for a file that is *not* in the world map.
        let missing_source = Source::new(missing_id, "Missing".to_string());

        let raw_main = source_main.root().span().into_raw().get();
        let raw_chap = source_chap.root().span().into_raw().get();
        let raw_missing = missing_source.root().span().into_raw().get();

        let mut sources = HashMap::new();
        sources.insert(main_id, source_main.clone());
        sources.insert(chap_id, source_chap.clone());
        let world = TestWorld::new(main_id, sources);

        let mut span_ids = HashSet::new();
        span_ids.insert(raw_main);
        span_ids.insert(raw_chap);
        span_ids.insert(raw_missing);

        let (span_ranges, source_files) =
            crate::resolve_span_ranges_and_source_files_for_span_ids(&world, &span_ids);

        let expected_main_hex = format!("{:x}", raw_main);
        let expected_chap_hex = format!("{:x}", raw_chap);
        let expected_missing_hex = format!("{:x}", raw_missing);

        let mut span_map = HashMap::new();
        for (span_hex, file_id, start, end) in span_ranges {
            span_map.insert(span_hex, (file_id, start, end));
        }

        assert!(
            !span_map.contains_key(&expected_missing_hex),
            "spans in unknown files must be skipped"
        );

        let main_span = Span::from_raw(NonZeroU64::new(raw_main).expect("main span must be nonzero"));
        let chap_span = Span::from_raw(NonZeroU64::new(raw_chap).expect("chap span must be nonzero"));

        let main_range = source_main
            .range(main_span)
            .expect("expected to resolve main span range");
        let chap_range = source_chap
            .range(chap_span)
            .expect("expected to resolve chap span range");

        assert_eq!(
            span_map.get(&expected_main_hex),
            Some(&(main_id.into_raw().get() as u32, main_range.start, main_range.end))
        );
        assert_eq!(
            span_map.get(&expected_chap_hex),
            Some(&(chap_id.into_raw().get() as u32, chap_range.start, chap_range.end))
        );

        let mut file_map = HashMap::new();
        for (file_id, path) in source_files {
            file_map.insert(file_id, path);
        }

        assert_eq!(
            file_map.get(&(main_id.into_raw().get() as u32)),
            Some(&"/main.typ".to_string())
        );
        assert_eq!(
            file_map.get(&(chap_id.into_raw().get() as u32)),
            Some(&"/chap1.typ".to_string())
        );
        assert!(
            !file_map.contains_key(&(missing_id.into_raw().get() as u32)),
            "sourceFiles must only include fileIds that were actually resolved"
        );
    }
}
