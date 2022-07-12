set WORKING_DIR=%CD%

cd %~dp0\server\beam\tau


SET MIX_ENV=dev

cmd /c mix local.hex --force
cmd /c mix local.rebar --force
cmd /c mix deps.get
cmd /c mix tailwind.install
cmd /c mix esbuild.install
cmd /c mix assets.deploy.prod

cd %WORKING_DIR%
