import React, { Component } from 'react'

import {Card, CardTitle, CardHeader} from 'material-ui/Card';
import {List, ListItem} from 'material-ui/List';
import Divider from 'material-ui/Divider';

import ClaimListItem from '../items/ClaimListItem';
import GetterHOC from '../hoc/GetterHOC'

const styles= {
  root: {
    height: '100%',
    width: '100%',
  }
}

const Feed = (props) => {
  const { claims, router } = props

  return (
    <div style={styles.root}>
      <Card>
        <CardTitle title='Feed'/>
        <Divider />
        <List>
          { (claims.length > 0) ?
              claims.map(claim =>
                <ClaimListItem
                  key={claim.id}
                  claim={claim}
                  router={router}
                />
              )
            :
              <ListItem primaryText={"No claims to show"} />
          }
        </List>
      </Card>
    </div>
  )
}

export default GetterHOC(
  Feed,
  (props) => ({
    claims: {
      path: '/',
      mapResponseToProps: (resp) => ({claims: resp})
    }
  })
)
