import { Text } from '@chakra-ui/react'

export interface LinkProps {
  id: string
}

export const Link = (props: LinkProps) => {
  const { id } = props

  return <Text>{id}</Text>
}
