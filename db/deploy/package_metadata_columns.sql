begin;

alter table packages
  add column requires_python text,
  add column core_metadata   text;

commit;
