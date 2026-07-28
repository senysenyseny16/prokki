begin;

alter table packages
  add column metadata_s3_key text,
  add column metadata_size   bigint;

commit;
