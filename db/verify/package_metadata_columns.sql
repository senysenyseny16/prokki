begin;

select requires_python, core_metadata from packages where false;

rollback;
