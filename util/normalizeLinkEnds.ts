import { OrgRoamLink } from '../api'
import { LinkObject } from 'force-graph'

export function normalizeLinkEnds(link: OrgRoamLink | LinkObject): [string, string] {
  // we need to cover both because force-graph modifies the original data
  // but if we supply the original data on each render, the graph will re-render sporadically
  const sourceId =
    typeof link.source === 'object' ? (link.source.id! as string) : (link.source as string)
  const targetId =
    typeof link.target === 'object' ? (link.target.id! as string) : (link.target as string)
  return [sourceId, targetId]
}
