import { initialColoring, initialVisuals } from '../components/config'
import { LinksByNodeId } from '../pages'
import { numberWithinRange } from './numberWithinRange'

export const getNodeColorById = ({
  id,
  linksByNodeId,
  visuals,
  coloring,
  cluster,
}: {
  id: string
  linksByNodeId: LinksByNodeId
  visuals: typeof initialVisuals
  cluster: any
  coloring: typeof initialColoring
}) => {
  const linklen = linksByNodeId[id!]?.length ?? 0
  if (coloring.method === 'degree') {
    return visuals.nodeColorScheme[
      numberWithinRange(linklen, 0, visuals.nodeColorScheme.length - 1)
    ]
  }
  return visuals.nodeColorScheme[linklen && cluster[id] % visuals.nodeColorScheme.length]
}
