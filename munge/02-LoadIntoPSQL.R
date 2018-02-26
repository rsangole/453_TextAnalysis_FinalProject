drv <- dbDriver("PostgreSQL")
con <- dbConnect(
    drv,
    dbname = "postgres",
    host = "localhost",
    port = 5433,
    user = "postgres",
    password = rstudioapi::askForPassword('Database password')
)

if (!dbExistsTable(con, "aviation_safety_training")) {
    # specifies the details of the table
    sql_command <-
        "CREATE TABLE
    aviation_safety_training(
    doc_id integer,
    text character varying(5000)
    )
    WITH (OIDS=FALSE)
    TABLESPACE pg_default;
    ALTER TABLE aviation_safety_training OWNER TO postgres;"
    dbGetQuery(con, sql_command)

    dbWriteTable(
        conn = con,
        name = 'aviation_safety_training',
        value = training_data,
        append = TRUE,
        row.names = FALSE
    )

}

if (!dbExistsTable(con, 'aviation_safety_training_labels')) {
    sql_command <-
        'CREATE TABLE public.aviation_safety_training_labels
    (
    doc_id integer,
    cat_a integer,
    cat_b integer,
    cat_c integer,
    cat_d integer,
    cat_e integer,
    cat_f integer,
    cat_g integer,
    cat_h integer,
    cat_i integer,
    cat_j integer,
    cat_k integer,
    cat_l integer,
    cat_m integer,
    cat_n integer,
    cat_o integer,
    cat_p integer,
    cat_q integer,
    cat_r integer,
    cat_s integer,
    cat_t integer,
    cat_u integer,
    cat_v integer
    )
    WITH ( OIDS = FALSE )
    TABLESPACE pg_default;
    ALTER TABLE public.aviation_safety_training_labels OWNER to postgres;'
    dbGetQuery(con, sql_command)

    dbWriteTable(
        conn = con,
        name = 'aviation_safety_training_labels',
        value = training_labels,
        append = TRUE,
        row.names = FALSE
    )
}

# close the connection
dbDisconnect(con)
