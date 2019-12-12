let Agent = ../../dhall/types/agent.dhall in
let Docker = ../../dhall/types/docker.dhall in
let DockerImage = ../../dhall/types/dockerimage.dhall in
  {
    agent = Agent.docker (
        Docker.image (
          DockerImage.custom { image = "myimage"
                             , label = Some "mylabel"
                             , args = Some "myargs"
                             }
          )
        )
  }
