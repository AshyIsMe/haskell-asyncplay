
initdb asyncplay -E utf-8
postgres -D asyncplay &
sleep 2

createdb asyncplay
psql asyncplay -f CreateDatabase.sql
