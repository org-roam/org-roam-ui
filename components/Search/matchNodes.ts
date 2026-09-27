import { OrgRoamNode } from '../../api'

export type SearchableNode = OrgRoamNode & {
  content: string
  headings: string[]
}

export type MatchCategory = 'title' | 'heading' | 'meta' | 'content'

export type NodeMatch = {
  node: SearchableNode
  category: MatchCategory
  words: string[]
  // The text to show/highlight for this match: the title, the matched
  // heading line, the matched tags/olp, or the matched content.
  snippet: string
}

const CATEGORY_RANK: Record<MatchCategory, number> = {
  title: 0,
  heading: 1,
  meta: 2,
  content: 3,
}

const HEADING_LINE = /^\*+\s+(.*)$/gm

export const extractHeadings = (content: string): string[] => {
  const headings: string[] = []
  const re = new RegExp(HEADING_LINE)
  let m: RegExpExecArray | null
  while ((m = re.exec(content))) {
    headings.push(m[1].trim())
  }
  return headings
}

// Plain, case-insensitive, multi-word AND substring matching -- not fuzzy.
// Fuse's fuzzy (edit-distance) matching against long note bodies surfaces a
// lot of loosely-related noise; a note's actual title/heading/content
// either contains the words you typed or it doesn't.
const containsAllWords = (haystack: string, words: string[]): boolean => {
  const lower = haystack.toLowerCase()
  return words.every((word) => lower.includes(word))
}

export const matchNode = (node: SearchableNode, words: string[]): NodeMatch | null => {
  if (containsAllWords(node.title, words)) {
    return { node, category: 'title', words, snippet: node.title }
  }
  const heading = node.headings.find((h) => containsAllWords(h, words))
  if (heading) {
    return { node, category: 'heading', words, snippet: heading }
  }
  const meta = [...node.tags, ...(node.olp ?? [])].join(' · ')
  if (meta && containsAllWords(meta, words)) {
    return { node, category: 'meta', words, snippet: meta }
  }
  if (containsAllWords(node.content, words)) {
    return { node, category: 'content', words, snippet: node.content }
  }
  return null
}

export const searchNodes = (nodes: SearchableNode[], query: string): NodeMatch[] => {
  const words = query.toLowerCase().trim().split(/\s+/).filter(Boolean)
  if (!words.length) {
    return []
  }
  return nodes
    .map((node) => matchNode(node, words))
    .filter((m): m is NodeMatch => !!m)
    .sort((a, b) => CATEGORY_RANK[a.category] - CATEGORY_RANK[b.category])
}
