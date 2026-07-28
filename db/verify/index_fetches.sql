begin;

select index_name, project, fetched_at
  from index_fetches where false;

rollback;
