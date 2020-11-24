#lang datalog

% Movies - https://x775.net/2019/03/18/Introduction-to-Datalog.html
genre("Pulp Fiction", "Crime").
genre("The Hateful Eight", "Crime").
genre("Resevoir Dogs", "Thriller").

starringIn("Pulp Fiction", "Tim Roth").
starringIn("The Hateful Eight", "Tim Roth").
starringIn("Resevoir Dogs", "Harvey Keitel").

directedBy("Pulp Fiction", "Quentin Tarantino").
directedBy("The Hateful Eight", "Quentin Tarantino").
directedBy("Resevoir Dogs", "Quentin Tarantino").

findMovie(Movie, Director, Star, Genre) :- genre(Movie, Genre),
                                           starringIn(Movie, Star),
                                           directedBy(Movie, Director).

findMovie(M, D, "Tim Roth", "Crime")?
