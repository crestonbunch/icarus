use thiserror::Error;

/// Represents a result from a Solr client
pub type SolrResult<T> = Result<T, SolrError>;

/// Represents an error from a Solr client
#[derive(Debug, Error)]
pub enum SolrError {
    #[error("Error in serialization")]
    SerializeError(#[from] serde_json::Error),
    #[error("Error in request")]
    ReqwestError(#[from] reqwest::Error),
}
