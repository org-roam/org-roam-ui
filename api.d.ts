export type OrgRoamGraphReponse = {
  nodes: OrgRoamNode[]
  links: OrgRoamLink[]
}

export type OrgRoamNode = {
  id: string
  file: string
  title: string
  level: number
  properties: {
    [key: string]: string | number
  }
}

export type OrgRoamLink = {
  source: string
  target: string
  type: string
}
