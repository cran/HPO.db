

--

-- HPO_DB schema

-- ====================

--

CREATE TABLE hpo_term (

  _id INTEGER PRIMARY KEY,

  hpo_id VARCHAR(12) NOT NULL UNIQUE,               -- DI ID

  term VARCHAR(255) NOT NULL                  -- textual label for the HPO term

);



CREATE TABLE hpo_synonym (

  _id INTEGER NOT NULL,                     -- REFERENCES hpo_term

  synonym VARCHAR(255) NOT NULL,                -- label or HPO ID

  secondary VARCHAR(12) NULL,                      -- HPO ID

  like_hpo_id SMALLINT,                          -- boolean (1 or 0)

  FOREIGN KEY (_id) REFERENCES hpo_term (_id)

);





CREATE TABLE hpo_parents ( 

  _id INTEGER NOT NULL,                     -- REFERENCES hpo_term

  _parent_id INTEGER NOT NULL,                   -- REFERENCES hpo_term

  relationship_type VARCHAR(7) NOT NULL,                 -- type of HPO child-parent relationship

  FOREIGN KEY (_id) REFERENCES hpo_term (_id),

  FOREIGN KEY (_parent_id) REFERENCES hpo_term (_id)

);



CREATE TABLE hpo_offspring (

  _id INTEGER NOT NULL,                     -- REFERENCES hpo_term

  _offspring_id INTEGER NOT NULL,                -- REFERENCES hpo_term

  FOREIGN KEY (_id) REFERENCES hpo_term (_id),

  FOREIGN KEY (_offspring_id) REFERENCES hpo_term (_id)

);





CREATE TABLE hpo_obsolete (

  hpo_id VARCHAR(12) PRIMARY KEY,                   -- HPO ID

  term VARCHAR(255) NOT NULL                   -- textual label for the HPO term

)

;



CREATE TABLE map_counts (

  map_name VARCHAR(80) PRIMARY KEY,

  count INTEGER NOT NULL

);



CREATE TABLE map_metadata (

  map_name VARCHAR(80) NOT NULL,

  source_name VARCHAR(80) NOT NULL,

  source_url VARCHAR(255) NOT NULL,

  source_date VARCHAR(20) NOT NULL

);



CREATE TABLE metadata (

  name VARCHAR(80) PRIMARY KEY,

  value VARCHAR(255)

);



-- Indexes

