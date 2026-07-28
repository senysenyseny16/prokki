begin;

create table index_fetches (
  index_name text        not null,
  project    text        not null,
  fetched_at timestamptz not null default now(),
  primary key (index_name, project)
);

commit;
