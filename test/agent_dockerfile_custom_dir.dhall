let Agent = constructors ../dhall/types/agent.dhall in
let Docker = constructors ../dhall/types/docker.dhall in
let DockerFile = constructors ../dhall/types/dockerfile.dhall in
  {
    agent = Agent.docker (
        Docker.file (
          DockerFile.custom { dir = "my/dir"
                            , filename = [] : Optional Text
                            , label = [] : Optional Text
                            , additionalBuildArgs = [] : Optional Text }))
  }
