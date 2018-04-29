let Agent = constructors ../dhall/types/agent.dhall in
let Docker = constructors ../dhall/types/docker.dhall in
let DockerImage = constructors ../dhall/types/dockerimage.dhall in
  {
    agent = Agent.docker (
        Docker.image (
          DockerImage.custom { image = "myimage"
                             , label = [] : Optional Text
                             , args = [] : Optional Text }
          )
        )
  }
