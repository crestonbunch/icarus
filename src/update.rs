use serde::Serialize;

#[derive(Debug, Serialize)]
#[serde(untagged)]
enum SolrUpdateOp<'a, D: Serialize> {
    Add(SolrAddRequest<D>),
    Delete(SolrDeleteRequest<'a>),
    Commit(SolrCommitRequest),
    Optimize(SolrOptimizeRequest),
    Rollback(SolrRollbackRequest),
}

/// A request to update the Solr index, e.g. by adding or removing documents,
/// committing uncommitted documents, or optimizing the index. Multiple
/// operations can be queued in a single update request.
///
/// ```
/// use serde_json::json;
///
/// let req = SolrUpdateRequest::new()
///     .add_many(vec![
///         SolrAddRequest::new(json!({"foo": "bar"})),
///         SolrAddRequest::new(json!({"hello": "world"})),
///     ])
///     .delete(SolrDeleteRequest::from_id("123"))
///     .commit(SolrCommitRequest::new());
/// ```
#[derive(Debug, Serialize, Default)]
pub struct SolrUpdateRequest<'a, D: Serialize> {
    #[serde(flatten)]
    #[serde(with = "serde_with::rust::tuple_list_as_map")]
    ops: Vec<(&'a str, SolrUpdateOp<'a, D>)>,
}

impl<'a, D: Serialize> SolrUpdateRequest<'a, D> {
    /// Create a new update request.
    pub fn new() -> Self {
        SolrUpdateRequest { ops: vec![] }
    }

    /// Add a request to insert or update a document in the index.
    pub fn add(mut self, req: SolrAddRequest<D>) -> Self {
        self.ops.push(("add", SolrUpdateOp::Add(req)));
        self
    }

    /// Add multiple requests to insert or update a documents in the index.
    pub fn add_many(mut self, reqs: Vec<SolrAddRequest<D>>) -> Self {
        self.ops
            .extend(reqs.into_iter().map(|r| ("add", SolrUpdateOp::Add(r))));
        self
    }

    /// Add a request to delete a document from the index.
    pub fn delete(mut self, req: SolrDeleteRequest<'a>) -> Self {
        self.ops.push(("delete", SolrUpdateOp::Delete(req)));
        self
    }

    /// Add multiple requests to delete a documents from the index.
    pub fn delete_many(mut self, reqs: Vec<SolrDeleteRequest<'a>>) -> Self {
        self.ops.extend(
            reqs.into_iter()
                .map(|r| ("delete", SolrUpdateOp::Delete(r))),
        );
        self
    }

    /// Add a request to commit uncomitted documents in the index.
    pub fn commit(mut self, req: SolrCommitRequest) -> Self {
        self.ops.push(("commit", SolrUpdateOp::Commit(req)));
        self
    }

    /// Add a request to optimize the index.
    pub fn optimize(mut self, req: SolrOptimizeRequest) -> Self {
        self.ops.push(("optimize", SolrUpdateOp::Optimize(req)));
        self
    }

    /// Add a request to rollback uncomitted documents in the index.
    pub fn rollback(mut self, req: SolrRollbackRequest) -> Self {
        self.ops.push(("rollback", SolrUpdateOp::Rollback(req)));
        self
    }
}

/// Represents a request to add an entry to Solr.
#[derive(Debug, Serialize)]
#[serde(rename = "add")]
pub struct SolrAddRequest<D: Serialize> {
    // TODO: does this support bulk adds as arrays?
    #[serde(rename = "commitWithin")]
    #[serde(skip_serializing_if = "Option::is_none")]
    commit_within: Option<usize>,
    #[serde(skip_serializing_if = "Option::is_none")]
    overwrite: Option<bool>,
    doc: D,
}

impl<D: Serialize> SolrAddRequest<D> {
    pub fn new(doc: D) -> SolrAddRequest<D> {
        SolrAddRequest {
            doc,
            commit_within: None,
            overwrite: None,
        }
    }

    /// Require that this document be committed within a certain number
    /// of seconds.
    pub fn commit_within(mut self, commit_within: usize) -> Self {
        self.commit_within = Some(commit_within);
        self
    }

    /// Explicitly set the overwrite behavior for this document. False
    /// means it will not overwrite an existing document with the same
    /// unique key.
    pub fn overwrite(mut self, overwrite: bool) -> Self {
        self.overwrite = Some(overwrite);
        self
    }
}

/// Represents a request to delete entries from Solr.
#[derive(Debug, Serialize)]
#[serde(untagged)]
pub enum SolrDeleteRequest<'a> {
    Id { id: &'a str },
    Ids { id: Vec<&'a str> },
    Query { query: &'a str },
}

impl<'a> SolrDeleteRequest<'a> {
    /// Make a request to delete a single document by its id
    pub fn from_id(id: &'a str) -> Self {
        SolrDeleteRequest::Id { id }
    }

    /// Make a request to delete a multiple documents by id
    pub fn from_ids(ids: Vec<&'a str>) -> Self {
        SolrDeleteRequest::Ids { id: ids }
    }

    /// Make a request to delete documents matching a query
    pub fn from_query(query: &'a str) -> Self {
        SolrDeleteRequest::Query { query }
    }
}

#[derive(Debug, Serialize, Default)]
pub struct SolrCommitRequest {
    #[serde(rename = "waitSearcher")]
    #[serde(skip_serializing_if = "Option::is_none")]
    wait_searcher: Option<bool>,
    #[serde(rename = "expungeDeletes")]
    #[serde(skip_serializing_if = "Option::is_none")]
    expunge_deletes: Option<bool>,
}

/// Represents a request to commit uncommitted documents in the index.
impl SolrCommitRequest {
    /// Create a new request to commit documents.
    pub fn new() -> Self {
        SolrCommitRequest::default()
    }

    /// Set whether or not to block on a new searcher, making the
    /// changes immediately visible.
    pub fn wait_searcher(mut self, wait_searcher: bool) -> Self {
        self.wait_searcher = Some(wait_searcher);
        self
    }

    /// Set whether or not to merge segments that have more than 10%
    /// deleted dogs, expunging the deleted documents in the process.
    pub fn expunge_deletes(mut self, expunge_deletes: bool) -> Self {
        self.expunge_deletes = Some(expunge_deletes);
        self
    }
}

/// Represents a request to optimize the index.
#[derive(Debug, Serialize, Default)]
pub struct SolrOptimizeRequest {
    #[serde(rename = "waitSearcher")]
    #[serde(skip_serializing_if = "Option::is_none")]
    wait_searcher: Option<bool>,
    #[serde(rename = "maxSegments")]
    #[serde(skip_serializing_if = "Option::is_none")]
    max_segments: Option<usize>,
}

impl SolrOptimizeRequest {
    /// Create a new request to optimize the index.
    pub fn new() -> Self {
        SolrOptimizeRequest::default()
    }

    /// Set whether or not to block on a new searcher, making the
    /// changes immediately visible.
    pub fn wait_searcher(mut self, wait_searcher: bool) -> Self {
        self.wait_searcher = Some(wait_searcher);
        self
    }

    /// Set the maximum segments.
    pub fn max_segments(mut self, max_segments: usize) -> Self {
        self.max_segments = Some(max_segments);
        self
    }
}

/// Represents a request to rollback uncommitted documents in the index.
#[derive(Debug, Serialize, Default)]
pub struct SolrRollbackRequest {}

impl SolrRollbackRequest {
    /// Create a new request to rollback uncommitted documents.
    pub fn new() -> Self {
        SolrRollbackRequest::default()
    }
}

#[cfg(test)]
mod tests {
    use serde_json::json;

    use super::*;
    use std::collections::HashMap;

    #[test]
    fn test_serialize_add() {
        let mut map = HashMap::new();
        map.insert("foo", "bar");
        let expect = json!({"doc": { "foo": "bar" }}).to_string();
        let actual = serde_json::to_string(&SolrAddRequest::new(map)).unwrap();
        assert_eq!(expect, actual);
    }

    #[test]
    fn test_serialize_delete() {
        let expect = json!({ "id": "abc123" }).to_string();
        let actual = serde_json::to_string(&SolrDeleteRequest::from_id("abc123")).unwrap();
        assert_eq!(expect, actual);

        let expect = json!({ "id": [ "abc", "123" ] }).to_string();
        let actual =
            serde_json::to_string(&SolrDeleteRequest::from_ids(vec!["abc", "123"])).unwrap();
        assert_eq!(expect, actual);

        let expect = json!({ "query": "abc123" }).to_string();
        let actual = serde_json::to_string(&SolrDeleteRequest::from_query("abc123")).unwrap();
        assert_eq!(expect, actual);
    }

    #[test]
    fn test_serialize_update() {
        let actual = serde_json::to_string(
            &SolrUpdateRequest::new()
                .add_many(vec![
                    SolrAddRequest::new(json!({"foo": "bar"})),
                    SolrAddRequest::new(json!({"hello": "world"})),
                ])
                .delete(SolrDeleteRequest::from_id("123"))
                .commit(SolrCommitRequest::new())
                .rollback(SolrRollbackRequest::new()),
        )
        .unwrap();
        let expect = "{\
            \"add\":{\"doc\":{\"foo\":\"bar\"}},\
            \"add\":{\"doc\":{\"hello\":\"world\"}},\
            \"delete\":{\"id\":\"123\"},\
            \"commit\":{},\
            \"rollback\":{}\
        }";
        assert_eq!(expect, actual);
    }
}
