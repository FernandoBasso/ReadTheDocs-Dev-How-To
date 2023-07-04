#!/usr/bin/env bash

#
# Dumps databases including their DDL code.
#
# NOTE: This is a simple script to dump my study databases. All the DBs
# must belong to the same user and with the the same password.
#
# Usage:
#
#   PGHOST=<host> \
#   PGUSER=<user> \
#   PGPASSWORD=<password> \
#   CONTAINER_NAME=<docker-container-name> \
#   bash dump_dbs.sh
#
# BEWARE: The password will be saved on your shell history. You may
# start the dump command with a space to cause bash to NOT save the
# command to the history, but keep in mind that it depends on some
# bash's configs (should be on by default, though, unless you changed
# it).
#

##
# Keep this array updated with whatever databases need to be backed up
# from time to time.
#
# BEWARE: Even though this is for local, study databases, be careful
# with sensitive information as the dumped DBs are committed to the repo
# as well.
#
dbs=(
	simplysql_cms_devel
	simplysql_teamsgames_devel
	simplysql_jointypes_devel
)

dumpsdir=./dbdumps
outfiles=()

mkdir -pv "$dumpsdir"

for db in "${dbs[@]}"
do
	outfile="${dumpsdir}/${db}.sql"
	outbkpfile=""

	##
	# Make a backup of the last backup in case the new backup
	# being made fails for some reason.
	#
	if [[ -r $outfile ]] ; then
		mv -v "$outfile" "${outfile%.sql}-bkp.sql"
	fi

	##
	# PGHOST, PGUSER and PGPASSWORD must be provided as env vars.
	#
	PGDATABASE="$db" docker exec "$CONTAINER_NAME" \
		pg_dump -U "$PGUSER" -C -Fp "$db" > "$outfile" &&
		outfiles+=("$outfile")
done

printf '\n\nSQL dumps created:\n\n'
for file in "${outfiles[@]}"
do
	printf '  • %s\n' "$file"
done

# vim: set tw=72:
