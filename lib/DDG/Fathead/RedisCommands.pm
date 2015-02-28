package DDG::Fathead::RedisCommands;

use DDG::Fathead;

primary_example_queries "redis rpush";

secondary_example_queries
    "info redis";

description "Redis command reference";

name "RedisCommands";

icon_url "/i/redis.io.ico";

source "Redis";

code_url "https://github.com/duckduckgo/zeroclickinfo-fathead/tree/master/share/fathead/redis_commands";

topics "geek", "sysadmin";

category "reference";

attribution
    github => ['https://github.com/djworth', 'djworth'],
    twitter => ['https://twitter.com/djworth', 'djworth'],
    web => ['http://danworth.com', 'Dan Worth'];

1;
