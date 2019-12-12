let Agent = ../../dhall/types/agent.dhall in
let Docker = ../../dhall/types/docker.dhall in
let DockerFile = ../../dhall/types/dockerfile.dhall in
  {
    agent = Agent.docker (
        Docker.file (
          DockerFile.custom { dir = "my/dir"
                            , filename = None Text
                            , label = None Text
                            , additionalBuildArgs = None Text }))
  }
