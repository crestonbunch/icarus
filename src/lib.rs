mod client;
mod error;
mod update;
mod url;

pub mod query;

pub use client::SolrClient;
pub use error::{SolrError, SolrResult};
pub use query::{SolrQueryRequest, SolrQueryResponse, SolrQueryResponseHeader, SolrQueryResult};
pub use update::{
    SolrAddRequest, SolrCommitRequest, SolrDeleteRequest, SolrOptimizeRequest, SolrRollbackRequest,
    SolrUpdateRequest,
};
