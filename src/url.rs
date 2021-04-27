use url::Url;

pub struct UrlBuilder {
    url: Url,
}

impl UrlBuilder {
    pub fn add_path_section(mut self, section: &str) -> Self {
        let path = self.url.path().trim_end_matches('/');
        let path = format!("{}/{}", path, section);
        self.url.set_path(&path);
        self
    }

    pub fn add_query_param(mut self, key: &str, value: &str) -> Self {
        {
            let params = &mut self.url.query_pairs_mut();
            params.append_pair(key, value).finish();
        }
        self
    }
}

impl From<Url> for UrlBuilder {
    fn from(url: Url) -> Self {
        UrlBuilder { url }
    }
}

impl From<UrlBuilder> for Url {
    fn from(builder: UrlBuilder) -> Self {
        builder.url
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add_paths() {
        let builder = UrlBuilder::from(Url::parse("https://example.com").unwrap());
        let actual = builder
            .add_path_section("foo")
            .add_path_section("bar")
            .into();
        let expect = Url::parse("https://example.com/foo/bar").unwrap();
        assert_eq!(expect, actual);
    }

    #[test]
    fn test_add_param() {
        let builder = UrlBuilder::from(Url::parse("https://example.com").unwrap());
        let actual = builder
            .add_query_param("foo", "bar")
            .add_query_param("hello", "world")
            .into();
        let expect = Url::parse("https://example.com?foo=bar&hello=world").unwrap();
        assert_eq!(expect, actual);
    }

    #[test]
    fn test_add_both() {
        let builder = UrlBuilder::from(Url::parse("https://example.com/foo?q=bar").unwrap());
        let actual = builder
            .add_path_section("hello")
            .add_query_param("p", "world")
            .into();
        let expect = Url::parse("https://example.com/foo/hello?q=bar&p=world").unwrap();
        assert_eq!(expect, actual);
    }
}
