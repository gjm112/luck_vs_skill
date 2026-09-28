

CREATE OR REPLACE TABLE mlb_statcast AS
SELECT *
FROM read_parquet('./parquet/mlb_statcast/*.parquet', union_by_name=TRUE);

CREATE OR REPLACE TABLE mlb_score_per_inning AS
WITH cumulative_score_per_inning AS (
  SELECT
    game_pk AS game_id,
    game_date,
    inning::INT AS inning,
    home_team,
    away_team,
    MAX(post_home_score) AS home_score,
    MAX(post_away_score) AS away_score
  FROM mlb_statcast
  WHERE game_type = 'R'
  GROUP BY
    game_pk,
    game_date,
    game_type,
    inning,
    home_team,
    away_team
)
SELECT
  game_id,
  game_date,
  inning,
  home_team,
  away_team,
  home_score,
  home_score - COALESCE(LAG(home_score) OVER (PARTITION BY game_id ORDER BY inning), 0) AS home_scored_in_inning, away_score,
  away_score - COALESCE(LAG(away_score) OVER (PARTITION BY game_id ORDER BY inning), 0) AS away_scored_in_inning
FROM cumulative_score_per_inning
ORDER BY
  game_id,
  inning;



CREATE OR REPLACE TABLE nfl_play_by_play AS
SELECT *
FROM read_parquet('./parquet/nfl_play_by_play/*.parquet', union_by_name=TRUE);

CREATE OR REPLACE TABLE nfl_score_per_qtr AS
WITH cumulative_score_per_qtr AS (
  SELECT 
    game_id,
    game_date,
    qtr::INT AS qtr,
    home_team,
    away_team,
    MAX(total_home_score) AS home_score,
    MAX(total_away_score) AS away_score
  FROM nfl_play_by_play
  WHERE season_type = 'REG'
  GROUP BY
    game_id,
    game_date,
    qtr,
    home_team,
    away_team
)
SELECT
  game_id,
  game_date,
  qtr,
  home_team,
  away_team,
  home_score - COALESCE(LAG(home_score) OVER (PARTITION BY game_id ORDER BY qtr), 0) AS home_scored_in_qtr,
  home_score,
  away_score - COALESCE(LAG(away_score) OVER (PARTITION BY game_id ORDER BY qtr), 0) AS away_scored_in_qtr,
  away_score
FROM cumulative_score_per_qtr
ORDER BY
  game_id,
  qtr;

