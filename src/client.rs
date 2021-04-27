use reqwest::Client;
use serde::{de::DeserializeOwned, Serialize};
use url::Url;

use crate::{
    query::{SolrQueryRequest, SolrQueryResult},
    update::SolrUpdateRequest,
    url::UrlBuilder,
    SolrResult,
};

/// Represents your API connection to Solr.
/// You use this struct to perform operations on Solr.
#[derive(Debug, Clone)]
pub struct SolrClient {
    client: reqwest::Client,
    // Base URL to connect to Solr. Should include the core.
    // For example http://localhost:8983/solr/production/
    base_url: Url,
    select_url: String,
    update_url: String,
    ping_url: String,
}

impl SolrClient {
    fn build_update_url(url: &Url) -> Url {
        let builder = UrlBuilder::from(url.clone());
        builder.add_path_section("update").into()
    }

    fn build_select_url(url: &Url) -> Url {
        let builder = UrlBuilder::from(url.clone());
        builder.add_path_section("select").into()
    }

    fn build_ping_url(url: &Url) -> Url {
        let builder = UrlBuilder::from(url.clone());
        builder
            .add_path_section("admin")
            .add_path_section("ping")
            .add_query_param("wt", "json")
            .into()
    }

    /// Creates a new instance of a SolrClient from an underlying
    /// reqwest client / connection pool. The URL should be the root
    /// of the Solr host URL.
    pub fn new(client: Client, url: &Url) -> SolrClient {
        SolrClient {
            client,
            base_url: url.clone(),
            select_url: SolrClient::build_select_url(url).into_string(),
            update_url: SolrClient::build_update_url(url).into_string(),
            ping_url: SolrClient::build_ping_url(url).into_string(),
        }
    }

    /// Make a request to update the Solr index.
    pub async fn update<'a, D: Serialize>(
        &self,
        req: &'a SolrUpdateRequest<'a, D>,
    ) -> SolrResult<()> {
        let resp = self.client.post(&self.update_url).json(req).send().await?;
        resp.error_for_status()?;
        Ok(())
    }

    pub async fn select<'a, D: DeserializeOwned>(
        &self,
        req: &'a SolrQueryRequest<'a>,
    ) -> SolrResult<SolrQueryResult<D>> {
        let resp = self.client.post(&self.select_url).json(req).send().await?;
        match resp.error_for_status() {
            Ok(resp) => Ok(resp.json::<SolrQueryResult<D>>().await?),
            Err(err) => Err(err.into()),
        }
    }
}
