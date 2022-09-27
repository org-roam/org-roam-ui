import { NodeObject, LinkObject } from 'force-graph'

export const isLinkRelatedToNode = (link: LinkObject, node: NodeObject | null) => {
  return (
    (link.source as NodeObject)?.id! === node?.id! || (link.target as NodeObject)?.id! === node?.id!
  )
}
