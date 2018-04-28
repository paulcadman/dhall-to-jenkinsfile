let Agent = ../dhall/types/agent.dhall
  in let Docker = ../dhall/types/docker.dhall
      in { agent = Agent.docker { image = "image", label = [] : Optional Text, args = [] : Optional Text } }
