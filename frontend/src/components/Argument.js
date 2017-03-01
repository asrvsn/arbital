import React, { Component, PropTypes } from 'react'

import {Card, CardActions, CardHeader, CardText} from 'material-ui/Card';
import FlatButton from 'material-ui/FlatButton';

import {List, ListItem} from 'material-ui/List';

const styles = {
  chip: {
    margin: 4,
  },
}

class Argument extends Component {

  render() {
    const { argument } = this.props

    // TODO
    const isMyArgument = undefined

    return (
      <Card expanded={true}>
        <CardHeader
          title={argument.summary}
          subtitle={argument.owner}
          actAsExpander={true}
          showExpandableButton={true}
        />
        { isMyArgument &&
          <CardActions>
            <FlatButton label="Update" />
            <FlatButton label="Delete" />
          </CardActions>
        }
        <CardText expandable={true}>
          <List>
            { argument.claimLinks.map(claimLink => {
                <ListItem primaryText={claimLink.body}
                          rightIcon={
                              <Chip onTouchTap={e =>
                                      this.handleAuthorPress(claimLink.authorId)
                                    }
                                    style={styles.chip}>
                                <Avatar size={32}>{claimLink.author[0]}</Avatar>
                                {claimLink.author}
                              </Chip>
                            } />
              })
            }
          </List>
        </CardText>
      </Card>
    )
  }

  handleAuthorPress(authorId) {
    //TODO
  }
}

export default Argument
