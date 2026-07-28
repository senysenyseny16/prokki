begin;

create extension if not exists pgcrypto;

create table schema_migrations (
  version     text primary key,
  applied_at  timestamptz not null default now()
);

commit;
