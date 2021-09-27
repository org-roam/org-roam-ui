import { Box, Heading } from '@chakra-ui/react'
import React from 'react'
import UniOrg from '../util/uniorg'
import getOrgText from '../util/getOrgText'

export interface PreviewProps {
  id: string
  title: string
}
export const Preview = (props: PreviewProps) => {
  const { id, title } = props
  const text = getOrgText(id)

  return (
    <Box>
      <Heading>{title}</Heading>
      <UniOrg orgText={text} />
    </Box>
  )
}
