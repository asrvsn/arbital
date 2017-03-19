import React, { Component } from 'react'

import Subheader from 'material-ui/Subheader';
import {List, ListItem} from 'material-ui/List';

import AuthoredListItem from './AuthoredListItem';
import GetterHOC from '../hoc/GetterHOC'

const styles= {
  root: {
    height: '100%',
    width: '100%',
  }
}

const Feed = (props) => {
  const {items} = props

  return (
    <div style={styles.root}>
      <List>
       <Subheader>Feed</Subheader>
        { (items == []) ?
          items.forEach(item => (
            <AuthoredListItem
              text={item.claimText}
              authorId={item.claimAuthorId}
              authorName={item.claimAuthorName}
              hrefPath={'/claims/' + item.claimId}
            />
          ))
          :
          <ListItem primaryText={"No items to show"} />
        }
      </List>
    </div>
  )
}

export default GetterHOC(
  Feed,
  (props) => ([
    {
      path: '/',
      mapResponseToProps: (resp) => ({items: resp})
    }
  ])
)
