import { LinksByNodeId, NodeById } from '../../pages/index'

import { GraphData, NodeObject, LinkObject } from 'force-graph'

import { normalizeLinkEnds } from '../../pages/index'
import { VStack, Box, Button, Heading, StackDivider } from '@chakra-ui/react'
import React from 'react'
import { ProcessedOrg } from '../../util/processOrg'

export interface BacklinksProps {
  previewNode: any
  setPreviewNode: any
  nodeById: NodeById
  linksByNodeId: LinksByNodeId
  getText: any
}

import { PreviewLink } from './Link'

export const Backlinks = (props: BacklinksProps) => {
  const { previewNode, setPreviewNode, nodeById, linksByNodeId, getText } = props
  const links = linksByNodeId[previewNode?.id] ?? []
  return (
    <Box>
      <Heading pt={4}>{'Backlinks (' + links.length + ')'}</Heading>
      <VStack
        pt={2}
        spacing={3}
        alignItems="start"
        divider={<StackDivider borderColor="gray.500" />}
        align="stretch"
        color="gray.800"
      >
        {previewNode?.id &&
          links.map((link: LinkObject, i: number) => {
            const [source, target] = normalizeLinkEnds(link)
            if (source === previewNode?.id) {
              return
            }
            return (
              <Box overflow="hidden" p={3} bg="gray.300" width="100%" key={source}>
                <PreviewLink
                  href={'id:' + source}
                  children={nodeById[source]?.title}
                  nodeById={nodeById}
                  setPreviewNode={setPreviewNode}
                  getText={getText}
                />
              </Box>
            )
          })}
      </VStack>
    </Box>
  )
}
