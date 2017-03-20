import React, { Component } from 'react'

import {Tabs, Tab} from 'material-ui/Tabs'
import {List, ListItem} from 'material-ui/List'

import AuthoredListItem from '../items/AuthoredListItem';
import GetterHOC from '../hoc/GetterHOC'

const User = (props) => {
  const { page, router } = props
  const { user, claims, args } = page

  const goToClaim = (claim) => router.push('/claims/' + claim.id)
  const goToArg = (arg) => router.push('/arguments/' + arg.id)

  return (
    <Tabs>
      <Tab label="Claims" >
        <List>
          { (claims.length > 0) ?
              claims.map(claim =>
                <AuthoredListItem
                  key={claim.id}
                  text={claim.text}
                  authorId={claim.authorId}
                  authorName={claim.authorName}
                  onTouchTap={e => goToClaim(claim)}
                  onAuthorTouchTap={e => {}}
                />
              )
            :
              <ListItem primaryText="(nothing)" />
          }
        </List>
      </Tab>
      <Tab label="Arguments" >
        <List>
          { args.map(arg =>
              <AuthoredListItem
                key={arg.id}
                text={arg.text}
                authorId={arg.authorId}
                authorName={arg.authorName}
                onTouchTap={e => goToArg(arg)}
                onAuthorTouchTap={e => {}}
              />
            )
          }
        </List>
      </Tab>
    </Tabs>
  )
}

export default GetterHOC(
  User,
  (props) => ({
    user: {
      path: props.location.pathname + '/page',
      mapResponseToProps: (resp) => ({page: resp})
    },
  })
)
