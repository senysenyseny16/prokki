begin;

alter table packages
  drop column requires_python,
  drop column core_metadata;

commit;
