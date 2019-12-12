let Agent = ../../dhall/types/agent.dhall in
let Docker = ../../dhall/types/docker.dhall in
let DockerFile = ../../dhall/types/dockerfile.dhall in
  {
    agent = Agent.docker (
      Docker.file (
        DockerFile.custom { dir = "my/dir"
                          , filename = Some "other.dockerfile"
                          , label = Some "mylabel"
                          , additionalBuildArgs = Some "-v l:l"
                          }
      )
    )
  }
