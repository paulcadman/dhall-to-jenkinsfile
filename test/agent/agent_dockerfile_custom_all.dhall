let Agent = constructors ../../dhall/types/agent.dhall in
let Docker = constructors ../../dhall/types/docker.dhall in
let DockerFile = constructors ../../dhall/types/dockerfile.dhall in
  {
    agent = Agent.docker (
        Docker.file (
          DockerFile.custom { dir = "my/dir"
                            , filename = ["other.dockerfile"] : Optional Text
                            , label = ["mylabel"] : Optional Text
                            , additionalBuildArgs = ["-v l:l"] : Optional Text }))
  }
