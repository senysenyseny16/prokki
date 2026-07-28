begin;

select id,
       index_name,
       project,
       filename,
       url_token,
       upstream_url,
       expected_sha256,
       s3_key,
       s3_etag,
       size,
       created_at,
       updated_at
  from packages
 where false;

rollback;
