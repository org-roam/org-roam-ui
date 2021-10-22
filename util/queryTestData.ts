import { Props } from './parseQuery'

export const testNode = {
  tags: ['definition', 'mathematics', 'topology'],
  properties: {
    CATEGORY: '20210513123625-monodromy',
    CTIME: '20210513123625',
    MTIME: '20210701200722',
    ID: 'e5951430-da06-4482-a7d3-7ac17c718d65',
    BLOCKED: '',
    FILE: '/Users/thomas/OneDrive/org-roam/subdir/20210513123625-monodromy.org',
    PRIORITY: 'B',
  },
  olp: null,
  pos: 1,
  level: 0,
  title: 'Monodromy',
  file: '/Users/thomas/OneDrive/org-roam/subdir/20210513123625-monodromy.org',
  id: 'e5951430-da06-4482-a7d3-7ac17c718d65',
  __indexColor: '#14007f',
  index: 126,
  x: 290.9872670921335,
  y: 202.1823995931162,
  vx: -2.4309686614458284e-19,
  vy: 1.4759459101795227e-19,
}
export type RealisticNode = typeof testNode
export function createTestNode(props: {
  [key: string]: string | number | string[] | Props
}): RealisticNode {
  return { ...testNode, ...props }
}
export const testdata = []
