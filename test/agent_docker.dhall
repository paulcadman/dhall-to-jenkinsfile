let Agent = ../dhall/types/agent.dhall
  in let Docker = ../dhall/types/docker.dhall
      in { agent = Agent.docker { image = "foo", label = ["A"] : Optional Text, args = ["a"] : Optional Text } }
