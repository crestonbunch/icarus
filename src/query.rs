use std::{
    fmt::{Display, Formatter},
    time::Duration,
};

use serde::{Deserialize, Serialize, Serializer};
use serde_with::{CommaSeparator, SerializeDisplay, SpaceSeparator};

/// Possible values for the "q.op" query parameter in Lucene queries.
#[derive(Debug, Serialize, Clone, Copy)]
#[serde(tag = "q.op")]
pub enum LuceneQueryOp {
    #[serde(rename = "AND")]
    And,
    #[serde(rename = "OR")]
    Or,
}

/// The key advantage of the standard query parser is that it supports a robust
/// and fairly intuitive syntax allowing you to create a variety of structured
/// queries. The largest disadvantage is that it’s very intolerant of syntax
/// errors, as compared with something like the DisMax query parser which is
/// designed to throw as few errors as possible.
///
/// [More info](https://solr.apache.org/guide/8_6/the-standard-query-parser.html)
#[serde_with::skip_serializing_none]
#[derive(Debug, Serialize, Default, Clone, Copy)]
pub struct LuceneQueryParams<'a> {
    q: &'a str,
    #[serde(flatten)]
    q_op: Option<LuceneQueryOp>,
    df: Option<&'a str>,
    sow: Option<bool>,
}

impl<'a> LuceneQueryParams<'a> {
    /// Defines a query using standard query syntax.
    pub fn new(q: &'a str) -> Self {
        Self {
            q,
            ..Self::default()
        }
    }

    /// Specifies the default operator for query expressions.
    /// Possible values are "AND" or "OR".
    pub fn q_op(mut self, op: LuceneQueryOp) -> Self {
        self.q_op = Some(op);
        self
    }

    /// Specifies a default searchable field.
    pub fn df(mut self, df: &'a str) -> Self {
        self.df = Some(df);
        self
    }

    /// Split on whitespace. If set to true, text analysis is invoked
    /// separately for each individual whitespace-separated term. The default
    /// is false; whitespace-separated term sequences will be provided to text
    /// analysis in one shot, enabling proper function of analysis filters that
    /// operate over term sequences, e.g., multi-word synonyms and shingles.
    pub fn sow(mut self, sow: bool) -> Self {
        self.sow = Some(sow);
        self
    }
}

/// Define a field boost for DisMax parameters: qf, pf. Serializes into a string
/// of the form `field^boost`.
#[derive(Debug, Default, Clone, Copy)]
pub struct DisMaxFieldBoost<'a> {
    field: &'a str,
    boost: Option<f32>,
}

impl<'a> DisMaxFieldBoost<'a> {
    pub fn new(field: &'a str) -> Self {
        Self {
            field,
            ..Self::default()
        }
    }

    pub fn with_boost(mut self, boost: f32) -> Self {
        self.boost = Some(boost);
        self
    }
}

impl<'a> Serialize for DisMaxFieldBoost<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self.boost {
            None => serializer.serialize_str(self.field),
            Some(boost) => serializer.serialize_str(&format!("{}^{}", self.field, boost)),
        }
    }
}

impl<'a> Display for DisMaxFieldBoost<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.boost {
            None => write!(f, "{}", self.field),
            Some(boost) => write!(f, "{}^{}", self.field, boost),
        }
    }
}

#[derive(Debug, SerializeDisplay, Clone, Copy)]
pub enum DisMaxMinShouldMatchUnit {
    /// An arbitrary integer.
    ///
    /// * Positive: Defines the minimum number of clauses that must match,
    ///   regardless of how many clauses there are in total.
    /// * Negative: Sets the minimum number of matching clauses to the total
    ///   number of optional clauses, minus this value.
    Absolute(isize),
    /// A float in the range [-1.0, 1.0].
    ///
    /// * Positive: Sets the minimum number of matching clauses to this
    ///   percentage of the total number of optional clauses. The number
    ///   computed from the percentage is rounded down and used as the minimum.
    /// * Negative: Indicates that this percent of the total number of optional
    ///   clauses can be missing. The number computed from the percentage is
    ///   rounded down, before being subtracted from the total to determine the
    ///   minimum number.
    Relative(f32),
}

impl Display for DisMaxMinShouldMatchUnit {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            DisMaxMinShouldMatchUnit::Absolute(v) => write!(f, "{}", v),
            DisMaxMinShouldMatchUnit::Relative(v) => write!(f, "{}%", v * 100.),
        }
    }
}

/// Defines a conditional expression indicating that if the number of optional
/// clauses is equal to (or less than) the integer, they are all required,
/// but if it’s greater than the integer, the specification applies.
#[derive(Debug, Clone, Copy)]
pub struct DisMaxMinShouldMatchExpression {
    left: usize,
    right: DisMaxMinShouldMatchUnit,
}

impl DisMaxMinShouldMatchExpression {
    pub fn new_abs(left: usize, right: isize) -> Self {
        Self {
            left,
            right: DisMaxMinShouldMatchUnit::Absolute(right),
        }
    }

    pub fn new_rel(left: usize, right: f32) -> Self {
        Self {
            left,
            right: DisMaxMinShouldMatchUnit::Relative(right),
        }
    }
}

impl Serialize for DisMaxMinShouldMatchExpression {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&format!("{}<{}", self.left, self.right))
    }
}

impl Display for DisMaxMinShouldMatchExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}<{}", self.left, self.right)
    }
}

/// A structured representation for the Minimum Should Match Parameter.
/// Serializes into a compatible string for a DisMax query.
#[derive(Debug, Serialize, Clone)]
#[serde(untagged)]
pub enum DisMaxMinShouldMatch {
    Unit(DisMaxMinShouldMatchUnit),
    #[serde(with = "serde_with::rust::StringWithSeparator::<SpaceSeparator>")]
    Expr(Vec<DisMaxMinShouldMatchExpression>),
}

/// The DisMax query parser is designed to process simple phrases (without
/// complex syntax) entered by users and to search for individual terms across
/// several fields using different weighting (boosts) based on the significance
/// of each field. Additional options enable users to influence the score based
/// on rules specific to each use case (independent of user input).
///
/// [More info](https://solr.apache.org/guide/8_6/the-dismax-query-parser.html)
#[serde_with::skip_serializing_none]
#[derive(Debug, Serialize, Default, Clone)]
pub struct DisMaxQueryParams<'a> {
    q: Option<&'a str>,
    #[serde(rename = "q.alt")]
    q_alt: Option<&'a str>,
    #[serde(with = "serde_with::rust::StringWithSeparator::<SpaceSeparator>")]
    #[serde(skip_serializing_if = "Vec::is_empty")]
    qf: Vec<DisMaxFieldBoost<'a>>,
    mm: Option<DisMaxMinShouldMatch>,
    #[serde(with = "serde_with::rust::StringWithSeparator::<SpaceSeparator>")]
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pf: Vec<DisMaxFieldBoost<'a>>,
    ps: Option<usize>,
    qs: Option<usize>,
    tie: Option<f32>,

    #[serde(skip_serializing_if = "Vec::is_empty")]
    bq: Vec<&'a str>,
    bf: Option<&'a str>,
}

impl<'a> DisMaxQueryParams<'a> {
    /// The q parameter defines the main "query" constituting the essence of
    /// the search. The parameter supports raw input strings provided by users
    /// with no special escaping. The + and - characters are treated as
    /// "mandatory" and "prohibited" modifiers for terms. Text wrapped in
    /// balanced quote characters (for example, "San Jose") is treated as a
    /// phrase. Any query containing an odd number of quote characters is
    /// evaluated as if there were no quote characters at all.
    pub fn new(q: &'a str) -> Self {
        Self {
            q: Some(q),
            ..Self::default()
        }
    }

    /// If specified, the q.alt parameter defines a query (which by default
    /// will be parsed using standard query parsing syntax) when the main q
    /// parameter is not specified or is blank. The q.alt parameter comes in
    /// handy when you need something like a query to match all documents
    /// (don’t forget &rows=0 for that one!) in order to get collection-wide
    /// faceting counts.
    pub fn q_alt(mut self, q_alt: &'a str) -> Self {
        self.q_alt = Some(q_alt);
        self
    }

    /// The qf parameter introduces a list of fields, each of which is assigned
    /// a boost factor to increase or decrease that particular field’s
    /// importance in the query. For example, the query below:
    ///
    ///     qf="fieldOne^2.3 fieldTwo fieldThree^0.4"
    ///
    /// assigns fieldOne a boost of 2.3, leaves fieldTwo with the default boost
    /// (because no boost factor is specified), and fieldThree a boost of 0.4.
    /// These boost factors make matches in fieldOne much more significant than
    /// matches in fieldTwo, which in turn are much more significant than
    /// matches in fieldThree
    pub fn add_qf(mut self, qf: DisMaxFieldBoost<'a>) -> Self {
        self.qf.push(qf);
        self
    }

    /// When processing queries, Lucene/Solr recognizes three types of clauses:
    /// mandatory, prohibited, and "optional" (also known as "should" clauses).
    /// By default, all words or phrases specified in the q parameter are
    /// treated as "optional" clauses unless they are preceded by a "+" or a
    /// "-". When dealing with these "optional" clauses, the mm parameter makes
    /// it possible to say that a certain minimum number of those clauses must
    /// match. The DisMax query parser offers great flexibility in how the
    /// minimum number can be specified.
    pub fn mm(mut self, mm: DisMaxMinShouldMatch) -> Self {
        self.mm = Some(mm);
        self
    }

    /// Once the list of matching documents has been identified using the fq
    /// and qf parameters, the pf parameter can be used to "boost" the score of
    /// documents in cases where all of the terms in the q parameter appear in
    /// close proximity.
    ///
    /// The format is the same as that used by the qf parameter: a list of
    /// fields and "boosts" to associate with each of them when making phrase
    /// queries out of the entire q parameter.
    pub fn add_pf(mut self, pf: DisMaxFieldBoost<'a>) -> Self {
        self.pf.push(pf);
        self
    }

    /// The ps parameter specifies the amount of "phrase slop" to apply to
    /// queries specified with the pf parameter. Phrase slop is the number of
    /// positions one token needs to be moved in relation to another token in
    /// order to match a phrase specified in a query.
    pub fn ps(mut self, ps: usize) -> Self {
        self.ps = Some(ps);
        self
    }

    /// The qs parameter specifies the amount of slop permitted on phrase
    /// queries explicitly included in the user’s query string with the qf
    /// parameter. As explained above, slop refers to the number of positions
    /// one token needs to be moved in relation to another token in order to
    /// match a phrase specified in a query.
    pub fn qs(mut self, qs: usize) -> Self {
        self.qs = Some(qs);
        self
    }

    /// The tie parameter specifies a float value (which should be something
    /// much less than 1) to use as tiebreaker in DisMax queries.
    ///
    /// When a term from the user’s input is tested against multiple fields,
    /// more than one field may match. If so, each field will generate a
    /// different score based on how common that word is in that field (for
    /// each document relative to all other documents). The tie parameter lets
    /// you control how much the final score of the query will be influenced by
    /// the scores of the lower scoring fields compared to the highest scoring
    /// field.
    ///
    /// A value of "0.0" - the default - makes the query a pure "disjunction
    /// max query": that is, only the maximum scoring subquery contributes to
    /// the final score. A value of "1.0" makes the query a pure "disjunction
    /// sum query" where it doesn’t matter what the maximum scoring sub query
    /// is, because the final score will be the sum of the subquery scores.
    /// Typically a low value, such as 0.1, is useful.
    pub fn tie(mut self, tie: f32) -> Self {
        self.tie = Some(tie);
        self
    }
}

#[derive(Debug, Serialize, Clone)]
#[serde(tag = "defType")]
pub enum Query<'a> {
    #[serde(rename = "lucene")]
    Lucene(LuceneQueryParams<'a>),
    #[serde(rename = "dismax")]
    DisMax(DisMaxQueryParams<'a>),
}

#[derive(Debug, Serialize, Clone, Copy)]
#[serde(untagged)]
enum Pagination<'a> {
    Offset {
        start: usize,
    },
    #[serde(rename_all = "camelCase")]
    Cursor {
        cursor_mark: &'a str,
    },
}

/// Create a request to query Solr for documents.
#[serde_with::skip_serializing_none]
#[derive(Debug, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct SolrQueryRequest<'a> {
    #[serde(flatten)]
    query: Query<'a>,
    #[serde(with = "serde_with::rust::StringWithSeparator::<CommaSeparator>")]
    #[serde(skip_serializing_if = "Vec::is_empty")]
    sort: Vec<String>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    filter: Vec<&'a str>,
    #[serde(with = "serde_with::rust::StringWithSeparator::<CommaSeparator>")]
    #[serde(skip_serializing_if = "Vec::is_empty")]
    fields: Vec<&'a str>,
    #[serde(flatten)]
    pagination: Option<Pagination<'a>>,
    rows: Option<usize>,
    time_allowed: Option<u128>,
}

#[derive(Debug, Default, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct SolrQueryResponseHeader {
    status: usize,
    zk_connected: bool,
    q_time: usize,
}

#[derive(Debug, Default, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct SolrQueryResponse<D> {
    pub num_found: usize,
    pub num_found_exact: bool,
    pub start: usize,
    pub max_score: Option<f32>,
    pub docs: Vec<D>,
}

#[derive(Debug, Default, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct SolrQueryResult<D> {
    pub response_header: SolrQueryResponseHeader,
    pub response: SolrQueryResponse<D>,
    pub next_cursor_mark: Option<String>,
}

impl<'a> SolrQueryRequest<'a> {
    /// Create a new query request using a simple string query. Any query
    /// syntax used in the input string is respected in the response query.
    pub fn new(query: Query<'a>) -> Self {
        SolrQueryRequest {
            query,
            sort: vec![],
            filter: vec![],
            fields: vec![],
            pagination: None,
            rows: None,
            time_allowed: None,
        }
    }

    /// Modify the request to sort the result by the given sort parameter in
    /// ascending order. Can be called multiple times to sort by multiple
    /// fields.
    pub fn sort_asc(mut self, field: &'a str) -> Self {
        let sort = format!("{} asc", field);
        self.sort.push(sort);
        self
    }

    /// Modify the request to sort the result by the given sort parameter in
    /// descending order. Can be called multiple times to sort by multiple
    /// fields.
    pub fn sort_desc(mut self, field: &'a str) -> Self {
        let sort = format!("{} desc", field);
        self.sort.push(sort);
        self
    }

    /// Request only rows starting from the given offset in the response. E.g.
    /// for pagination. Multiple calls to start() will overwrite the last
    /// value set. It will also replace any early calls to cursor_mark().
    pub fn start(mut self, offset: usize) -> Self {
        self.pagination = Some(Pagination::Offset { start: offset });
        self
    }

    /// Request only rows starting from the given cursor mark. E.g. for
    /// pagination. Multiple calls to cursor_mark() will overwrite the last
    /// value set. It will also replace any early calls to start().
    pub fn cursor_mark(mut self, mark: &'a str) -> Self {
        self.pagination = Some(Pagination::Cursor { cursor_mark: mark });
        self
    }

    /// Request only the given number of rows in the response. E.g. for
    /// pagination. Multiple calls to rows() will overwrite the last
    /// value set.
    pub fn rows(mut self, rows: usize) -> Self {
        self.rows = Some(rows);
        self
    }

    /// The fq parameter defines a query that can be used to restrict the
    /// superset of documents that can be returned, without influencing score.
    /// It can be very useful for speeding up complex queries, since the
    /// queries specified with fq are cached independently of the main query.
    /// When a later query uses the same filter, there’s a cache hit, and filter
    /// results are returned quickly from the cache.
    pub fn add_filter(mut self, filter: &'a str) -> Self {
        self.filter.push(filter);
        self
    }

    /// The fl parameter limits the information included in a query response to
    /// a specified list of fields. The fields must be either stored="true" or
    /// docValues="true"`.`
    ///
    /// The field list can be specified as a space-separated or comma-separated
    /// list of field names. The string "score" can be used to indicate that the
    /// score of each document for the particular query should be returned as a
    /// field. The wildcard character * selects all the fields in the document
    /// which are either stored="true" or docValues="true" and
    /// useDocValuesAsStored="true" (which is the default when docValues are
    /// enabled). You can also add pseudo-fields, functions and transformers to
    /// the field list request.
    pub fn add_field(mut self, field: &'a str) -> Self {
        self.fields.push(field);
        self
    }

    /// This parameter specifies the amount of time, in milliseconds, allowed
    /// for a search to complete. If this time expires before the search is
    /// complete, any partial results will be returned, but values such as
    /// numFound, facet counts, and result stats may not be accurate for the
    /// entire result set. In case of expiration, if omitHeader isn’t set to
    /// true the response header contains a special flag called partialResults.
    pub fn time_allowed(mut self, time_allowed: Duration) -> Self {
        self.time_allowed = Some(time_allowed.as_millis());
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_query() {
        let q = Query::DisMax(DisMaxQueryParams::new("test"));
        let actual = serde_json::to_string(&SolrQueryRequest::new(q)).unwrap();
        let expect = "{\"defType\":\"dismax\",\"q\":\"test\"}";
        assert_eq!(expect, actual);
    }

    #[test]
    fn test_sort() {
        let q = Query::Lucene(LuceneQueryParams::new("test"));
        let actual =
            serde_json::to_string(&SolrQueryRequest::new(q).sort_asc("one").sort_desc("two"))
                .unwrap();
        let expect = "{\"defType\":\"lucene\",\"q\":\"test\",\"sort\":\"one asc,two desc\"}";
        assert_eq!(expect, actual);
    }

    #[test]
    fn test_start() {
        let q = Query::Lucene(LuceneQueryParams::new("test"));
        let actual = serde_json::to_string(&SolrQueryRequest::new(q).start(5).rows(10)).unwrap();
        let expect = "{\"defType\":\"lucene\",\"q\":\"test\",\"start\":5,\"rows\":10}";
        assert_eq!(expect, actual);
    }

    #[test]
    fn test_cursor_mark() {
        let q = Query::Lucene(LuceneQueryParams::new("test"));
        let actual =
            serde_json::to_string(&SolrQueryRequest::new(q).cursor_mark("abc").rows(10)).unwrap();
        let expect = "{\"defType\":\"lucene\",\"q\":\"test\",\"cursorMark\":\"abc\",\"rows\":10}";
        assert_eq!(expect, actual);
    }

    #[test]
    fn test_filter() {
        let q = Query::Lucene(LuceneQueryParams::new("test"));
        let actual = serde_json::to_string(
            &SolrQueryRequest::new(q)
                .add_filter("popularity:[10 TO *]")
                .add_filter("section:0"),
        )
        .unwrap();
        let expect = "{\"defType\":\"lucene\",\"q\":\"test\",\"filter\":[\"popularity:[10 TO *]\",\"section:0\"]}";
        assert_eq!(expect, actual);
    }

    #[test]
    fn test_fields() {
        let q = Query::Lucene(LuceneQueryParams::new("test"));
        let actual = serde_json::to_string(
            &SolrQueryRequest::new(q)
                .add_field("id")
                .add_field("name")
                .add_field("score"),
        )
        .unwrap();
        let expect = "{\"defType\":\"lucene\",\"q\":\"test\",\"fields\":\"id,name,score\"}";
        assert_eq!(expect, actual);
    }

    #[test]
    fn test_time_allowed() {
        let q = Query::Lucene(LuceneQueryParams::new("test"));
        let actual =
            serde_json::to_string(&SolrQueryRequest::new(q).time_allowed(Duration::from_secs(1)))
                .unwrap();
        let expect = "{\"defType\":\"lucene\",\"q\":\"test\",\"timeAllowed\":1000}";
        assert_eq!(expect, actual);
    }

    #[test]
    fn test_dismax_q_alt() {
        let q = Query::DisMax(DisMaxQueryParams::new("").q_alt("test"));
        let actual = serde_json::to_string(&SolrQueryRequest::new(q)).unwrap();
        let expect = "{\"defType\":\"dismax\",\"q\":\"\",\"q.alt\":\"test\"}";
        assert_eq!(expect, actual);
    }

    #[test]
    fn test_dismax_qf() {
        let q = Query::DisMax(
            DisMaxQueryParams::new("test")
                .add_qf(DisMaxFieldBoost::new("fieldOne").with_boost(2.3))
                .add_qf(DisMaxFieldBoost::new("fieldTwo"))
                .add_qf(DisMaxFieldBoost::new("fieldThree").with_boost(0.4)),
        );
        let req = SolrQueryRequest::new(q);
        let actual = serde_json::to_string(&req).unwrap();
        let expect = "{\"defType\":\"dismax\",\"q\":\"test\",\"qf\":\"fieldOne^2.3 fieldTwo fieldThree^0.4\"}";
        assert_eq!(expect, actual);
    }

    #[test]
    fn test_dismax_mm() {
        let q = Query::DisMax(
            DisMaxQueryParams::new("test").mm(DisMaxMinShouldMatch::Unit(
                DisMaxMinShouldMatchUnit::Absolute(3),
            )),
        );
        let req = SolrQueryRequest::new(q);
        let actual = serde_json::to_string(&req).unwrap();
        let expect = "{\"defType\":\"dismax\",\"q\":\"test\",\"mm\":\"3\"}";
        assert_eq!(expect, actual);

        let q = Query::DisMax(
            DisMaxQueryParams::new("test").mm(DisMaxMinShouldMatch::Unit(
                DisMaxMinShouldMatchUnit::Absolute(-2),
            )),
        );
        let req = SolrQueryRequest::new(q);
        let actual = serde_json::to_string(&req).unwrap();
        let expect = "{\"defType\":\"dismax\",\"q\":\"test\",\"mm\":\"-2\"}";
        assert_eq!(expect, actual);

        let q = Query::DisMax(
            DisMaxQueryParams::new("test").mm(DisMaxMinShouldMatch::Unit(
                DisMaxMinShouldMatchUnit::Relative(0.75),
            )),
        );
        let req = SolrQueryRequest::new(q);
        let actual = serde_json::to_string(&req).unwrap();
        let expect = "{\"defType\":\"dismax\",\"q\":\"test\",\"mm\":\"75%\"}";
        assert_eq!(expect, actual);

        let q = Query::DisMax(
            DisMaxQueryParams::new("test").mm(DisMaxMinShouldMatch::Unit(
                DisMaxMinShouldMatchUnit::Relative(-0.25),
            )),
        );
        let req = SolrQueryRequest::new(q);
        let actual = serde_json::to_string(&req).unwrap();
        let expect = "{\"defType\":\"dismax\",\"q\":\"test\",\"mm\":\"-25%\"}";
        assert_eq!(expect, actual);

        let q = Query::DisMax(
            DisMaxQueryParams::new("test").mm(DisMaxMinShouldMatch::Expr(vec![
                DisMaxMinShouldMatchExpression::new_rel(3, 0.9),
            ])),
        );
        let req = SolrQueryRequest::new(q);
        let actual = serde_json::to_string(&req).unwrap();
        let expect = "{\"defType\":\"dismax\",\"q\":\"test\",\"mm\":\"3<90%\"}";
        assert_eq!(expect, actual);

        let q = Query::DisMax(
            DisMaxQueryParams::new("test").mm(DisMaxMinShouldMatch::Expr(vec![
                DisMaxMinShouldMatchExpression::new_rel(2, -0.25),
                DisMaxMinShouldMatchExpression::new_abs(9, -3),
            ])),
        );
        let req = SolrQueryRequest::new(q);
        let actual = serde_json::to_string(&req).unwrap();
        let expect = "{\"defType\":\"dismax\",\"q\":\"test\",\"mm\":\"2<-25% 9<-3\"}";
        assert_eq!(expect, actual);
    }

    #[test]
    fn test_dismax_pf() {
        let q =
            Query::DisMax(DisMaxQueryParams::new("test").add_pf(DisMaxFieldBoost::new("category")));
        let req = SolrQueryRequest::new(q);
        let actual = serde_json::to_string(&req).unwrap();
        let expect = "{\"defType\":\"dismax\",\"q\":\"test\",\"pf\":\"category\"}";
        assert_eq!(expect, actual);
    }

    #[test]
    fn test_dismax_ps_qs_tie() {
        let q = Query::DisMax(DisMaxQueryParams::new("test").ps(1).qs(2).tie(0.1));
        let req = SolrQueryRequest::new(q);
        let actual = serde_json::to_string(&req).unwrap();
        let expect = "{\"defType\":\"dismax\",\"q\":\"test\",\"ps\":1,\"qs\":2,\"tie\":0.1}";
        assert_eq!(expect, actual);
    }

    #[test]
    fn test_lucene_q_op() {
        let q = Query::Lucene(LuceneQueryParams::new("").q_op(LuceneQueryOp::And));
        let actual = serde_json::to_string(&SolrQueryRequest::new(q)).unwrap();
        let expect = "{\"defType\":\"lucene\",\"q\":\"\",\"q.op\":\"AND\"}";
        assert_eq!(expect, actual);
    }

    #[test]
    fn test_lucene_df() {
        let q = Query::Lucene(LuceneQueryParams::new("").df("category"));
        let actual = serde_json::to_string(&SolrQueryRequest::new(q)).unwrap();
        let expect = "{\"defType\":\"lucene\",\"q\":\"\",\"df\":\"category\"}";
        assert_eq!(expect, actual);
    }

    #[test]
    fn test_lucene_sow() {
        let q = Query::Lucene(LuceneQueryParams::new("").sow(true));
        let actual = serde_json::to_string(&SolrQueryRequest::new(q)).unwrap();
        let expect = "{\"defType\":\"lucene\",\"q\":\"\",\"sow\":true}";
        assert_eq!(expect, actual);
    }
}
