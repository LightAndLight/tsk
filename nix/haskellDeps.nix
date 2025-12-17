let
  sage-src = {
    type = "github";
    owner = "LightAndLight";
    repository = "sage";
    commit = "c449a0eb061e1b9826b692e425ee86b85f4607d5";
  };
in
{
  diagnostica = {
    type = "github";
    owner = "LightAndLight";
    repository = "diagnostica";
    commit = "ae00004b101bde236abedba7ce480d289707356d";
  };
  diagnostica-sage = {
    type = "github";
    owner = "LightAndLight";
    repository = "diagnostica-sage";
    commit = "431c636174a5c39f1a3bfc1141bf7d95c0b5b399";
  };
  generics-eot = {
    type = "github";
    owner = "soenkehahn";
    repository = "generics-eot";
    # branch: sh/ghc-9.6
    commit = "351f0c2e8d4586d294f803e906c06b8dcc7a0a14";
  };
  sage = {
    inherit (sage-src) type owner repository commit;
    directory = "sage";
  };
  sage-parsers-instances = {
    inherit (sage-src) type owner repository commit;
    directory = "sage-parsers-instances";
  };
}
