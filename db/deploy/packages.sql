begin;

create table packages (
  id              bigserial primary key,

  index_name      text not null,
  project         text not null,
  filename        text not null,
  url_token       text not null,
  upstream_url    text not null,
  expected_sha256 text,

  s3_key          text,
  s3_etag         text,
  size            bigint,

  created_at      timestamptz not null default now(),
  updated_at      timestamptz not null default now(),

  unique (index_name, url_token)
);

create index idx_packages_project on packages (index_name, project);

commit;
