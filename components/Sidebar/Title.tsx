import { Flex, Heading } from '@chakra-ui/react'
import React from 'react'

import { OrgRoamNode } from '../../api'
export interface TitleProps {
  previewNode: OrgRoamNode | undefined
}

export const Title = (props: TitleProps) => {
  const { previewNode } = props
  return (
    <Flex maxW="90%">
      {/* <BiFile
       * // onContextMenu={(e) => {
       * //   e.preventDefault()
       * //   openContextMenu(previewNode, e)
       * // }}
       * /> */}
      <Heading lineHeight={1.2} size="md" fontWeight={600} pt={4}>
        {previewNode?.title}
      </Heading>
    </Flex>
  )
}
