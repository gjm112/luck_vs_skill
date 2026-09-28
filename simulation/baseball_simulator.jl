using DataFrames
using Distributions
using Parquet

function simulate_games(
    team_i_marker::Int64,
    team_j_marker::Int64,
    team_skill_i_mean::Float64,
    team_skill_i_variance::Float64,
    team_skill_j_mean::Float64,
    team_skill_j_variance::Float64,
    game_luck_variance::Float64, 
    innings::Int64, 
    total_games::Int64
)
  games = DataFrame(
    game_id = Int64[],
    inning = Int64[],
    team_i_marker = Int64[],
    team_j_marker = Int64[],
    winning_team = String[],
    team_i_points_scored = Int64[],
    team_j_points_scored = Int64[],
    team_i_running_score = Int64[],
    team_j_running_score = Int64[],

    team_skill_i_mean = Float64[],
    team_skill_i_variance = Float64[],
    team_skill_j_mean = Float64[],
    team_skill_j_variance = Float64[],
    game_luck_variance = Float64[]

  )
  game_luck_dist = Normal(0, sqrt(game_luck_variance))
  skill_i_dist = Normal(team_skill_i_mean, sqrt(team_skill_i_variance))
  skill_j_dist = Normal(team_skill_j_mean, sqrt(team_skill_j_variance))
  for game_id in 1:total_games
    team_i_running_score::Int64 = 0
    team_j_running_score::Int64 = 0
    skill_signal_i = rand(skill_i_dist)
    skill_signal_j = rand(skill_j_dist)
    for inning in 1:innings

        game_luck_noise_i = rand(game_luck_dist)
        game_luck_noise_j = rand(game_luck_dist)

        lambda_i = exp(skill_signal_i + game_luck_noise_i)
        lambda_j = exp(skill_signal_j + game_luck_noise_j)
        team_i_points_scored = rand(Poisson(lambda_i))
        team_j_points_scored = rand(Poisson(lambda_j))

        team_i_running_score = team_i_running_score + team_i_points_scored
        team_j_running_score = team_j_running_score + team_j_points_scored

        if team_i_running_score > team_j_running_score
          winning_team = string(team_i_marker)
        elseif team_i_running_score == team_j_running_score
          winning_team = "tie"
        else
          winning_team = string(team_j_marker)
        end

        inning_state = (
            game_id,
            inning,
            team_i_marker,
            team_j_marker,
            winning_team,
            team_i_points_scored,
            team_j_points_scored,
            team_i_running_score,
            team_j_running_score,
            team_skill_i_mean,
            team_skill_i_variance,
            team_skill_j_mean,
            team_skill_j_variance,
            game_luck_variance
        )
        push!(games, inning_state)
    end
  end
  games
end

function simulate_baseball_season(
  teams_skills_means::Vector{Float64},
  teams_skills_variances::Vector{Float64},
  game_luck_variance::Float64,
  innings_per_match::Int64,
  num_required_matches::Int64,
  extra_games::Int64
)
  games = []
  for _ in 1:extra_games
    for (team_i_marker, (team_skill_i_mean, team_skill_i_variance)) in enumerate(zip(teams_skills_means, teams_skills_variances))
      for (team_j_marker, (team_skill_j_mean, team_skill_j_variance)) in enumerate(zip(teams_skills_means, teams_skills_variances))
        if team_i_marker != team_j_marker
          paired_games = simulate_games(
            team_i_marker,
            team_j_marker,
            team_skill_i_mean,
            team_skill_i_variance,
            team_skill_j_mean,
            team_skill_j_variance,
            game_luck_variance,
            innings_per_match,
            num_required_matches)
          push!(games, paired_games)
        end
      end
    end
  end
  reduce(vcat, games)
end


games = simulate_baseball_season([0.1, 0.5, 1.0], [0.1, 0.5, 1.0], 1.0, 9, 3, 2_000)

write_parquet("simulated_games.parquet", games)
