import { Text } from '@chakra-ui/react'

// Highlights every (case-insensitive, non-overlapping) occurrence of any of
// WORDS in TEXT. Plain substring search, not fuzzy -- see SearchContent.tsx
// for why.
export const highlightWords = (text: string, words: string[]) => {
  if (!words.length) {
    return text
  }
  const lower = text.toLowerCase()
  const ranges: [number, number][] = []
  words.forEach((word) => {
    if (!word) {
      return
    }
    let from = 0
    while (from <= lower.length) {
      const idx = lower.indexOf(word, from)
      if (idx === -1) {
        break
      }
      ranges.push([idx, idx + word.length])
      from = idx + word.length
    }
  })
  if (!ranges.length) {
    return text
  }
  ranges.sort((a, b) => a[0] - b[0])
  const merged: [number, number][] = []
  ranges.forEach(([start, end]) => {
    const last = merged[merged.length - 1]
    if (last && start <= last[1]) {
      last[1] = Math.max(last[1], end)
    } else {
      merged.push([start, end])
    }
  })

  const children: React.ReactNode[] = []
  let cursor = 0
  merged.forEach(([start, end], i) => {
    children.push(text.substring(cursor, start))
    children.push(
      <Text key={i} as="u">
        {text.substring(start, end)}
      </Text>,
    )
    cursor = end
  })
  children.push(text.substring(cursor))

  return <>{children}</>
}
