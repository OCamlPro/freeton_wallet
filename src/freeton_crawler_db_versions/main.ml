
let database () = "ftc"

let versions = [

  0,
  [  (* from 0 to 1 *)

    {|
CREATE TABLE freeton_events(
    serial SERIAL PRIMARY KEY,
    msg_id VARCHAR NOT NULL UNIQUE,
    event_name VARCHAR NOT NULL,
    event_args VARCHAR NOT NULL,
    time BIGINT NOT NULL
    )|};

    {|
CREATE TABLE freeton_transactions(
    lt BIGINT PRIMARY KEY,
    id VARCHAR NOT NULL,
    block_id VARCHAR NOT NULL,
    json VARCHAR NOT NULL
    )|};

    {|
    CREATE TABLE pids (
      name VARCHAR PRIMARY KEY,
      pid INTEGER NOT NULL
    )
|}

  ],
  [ (* from 1 to 0 *)
    {|DROP TABLE freeton_transactions|};
    {|DROP TABLE freeton_events|};
    {|DROP TABLE pids|};
  ]

]

let downgrades =
  List.map (fun (num, _upgrades, downgrades) -> (num, downgrades)) versions

let upgrades =
  List.map (fun (version, upgrade, downgrade) ->
      let upgrade dbh version =
        EzPG.upgrade ~dbh ~version ~downgrade upgrade;
      in
      (version, upgrade)
    ) versions
