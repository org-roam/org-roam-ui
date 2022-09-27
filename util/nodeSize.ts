import { filter } from '@chakra-ui/react'
import { initialVisuals } from '../components/config'
import { LinksByNodeId } from '../pages'
import { NodeObject } from 'force-graph'

export const nodeSize = ({
  linksByNodeId,
  visuals,
  highlightedNodes,
  previouslyHighlightedNodes,
  opacity,
  node,
}: {
  node: NodeObject
  visuals: typeof initialVisuals

  highlightedNodes: Record<string, any>
  previouslyHighlightedNodes: Record<string, any>
  opacity: number
  linksByNodeId: LinksByNodeId
}) => {
  const links = linksByNodeId[node.id!] ?? []
  const parentNeighbors = links.length ? links.filter((link) => link.type === 'parent').length : 0
  const basicSize =
    3 + links.length * visuals.nodeSizeLinks - (!filter.parent ? parentNeighbors : 0)
  if (visuals.highlightNodeSize === 1) {
    return basicSize
  }
  const highlightSize =
    highlightedNodes[node.id!] || previouslyHighlightedNodes[node.id!]
      ? 1 + opacity * (visuals.highlightNodeSize - 1)
      : 1
  return basicSize * highlightSize
}
