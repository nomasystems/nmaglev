{logdir, "log"}.
{config, "test.cfg"}.
{alias, test, ".."}.
{suites, test, all}.
{ct_hooks, [{cth_surefire, [{path, "report.xml"}]}]}.
