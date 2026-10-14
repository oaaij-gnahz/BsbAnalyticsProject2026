# Baseball analytics project ideas

Below I list 5 tentative ideas that interest me and some overall descriptions.

## How to measure pitchers' talents in inducing "weak" contact?
  * Model the correlation between pitch tracking data and batted ball tracking data
  * How much true talent is in the player's ability to generate relatively consistent batted ball trajectories that have low xLW3.
## How to evaluate relievers when entering the game in the middle of an inning?
  * Consider the importance of the situation, relative win expectancy of both teams when the reliever enters the game.
  * Similar concept has been explored with leverage index, win probability added and FanGraph articles (https://blogs.fangraphs.com/we-should-account-for-inherited-runners-better/ for example)
## On the "streakiness" of offensive performances
  * Rationale: A good position player almost always plays great defense, but their bats can get hot or cold. Therefore the streakiness on the offensive side is interesting.
  * Between consecutive games, are offensive runs scored independent or correlated?
  * Can potentially use a Markov model to evaluate temporal correlations at different timescales
  * Compare it against a baseline: even repeating dice throwing can give us observations of long or short streaks.
## How much does "situational hitting" impacts the game?
  * Rationale: It happens to us fans that, in a close game, late inning, 1 out and runners in scoring position, we would hope that our team's batter would "put the ball in play" and get disappointed by a flyout. How much does a players use "situational hitting", and how much control does a player actually have?
  * On surface level, we can bucket players' batted ball tracking data by base-out state.
  * Can we dive deeper and find patterns between batted ball trajectory and base-out states?
## Can we predict when a pitcher should adjust their pitch usage before the batters catch up?
  * Rationale: in 2026 RHP Jacob Webb was the Cubs' closer from around May to late August, and used his changeup to fool batters frequently. From September on, batters started to do damage against his changeup. Physical fatigue might be a factor, but batters can also come up with better strategies against a pitcher over time, especially when technologies that simulate pitchers' delivery and pitch movement.
  * Can we study batted ball tracking data and swing decision data over time, and understand how and when batters are gaining advantage over the pitcher
