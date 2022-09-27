import { LinksByNodeId } from '../pages'
import { normalizeLinkEnds } from './normalizeLinkEnds'

export const findNthNeighbors = ({
  ids,
  excludedIds,
  n,
  linksByNodeId,
}: {
  ids: string[]
  excludedIds: string[]
  n: number
  linksByNodeId: LinksByNodeId
}) => {
  let queue = [ids[0]]
  let todo: string[] = []
  const completed = [ids[0]]
  Array.from({ length: n }, () => {
    queue.forEach((node) => {
      const links = linksByNodeId[node as string] ?? []
      links.forEach((link) => {
        const [sourceId, targetId] = normalizeLinkEnds(link)
        if (excludedIds.some((id) => [sourceId, targetId].includes(id))) {
          return
        }
        if (!completed.includes(sourceId)) {
          todo.push(sourceId)
          return
        }
        if (!completed.includes(targetId)) {
          todo.push(targetId)
          return
        }
        return
      })
    })
    queue = todo
    todo.forEach((neighbor) => neighbor && completed.push(neighbor))
    todo = []
  })
  return completed
}
