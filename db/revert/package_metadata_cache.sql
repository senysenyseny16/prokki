begin;

alter table packages
  drop column metadata_s3_key,
  drop column metadata_size;

commit;
